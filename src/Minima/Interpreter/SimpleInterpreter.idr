module SimpleInterpreter

import Minima.AST
import Minima.Annotators.Annotations
import Minima.Interpreter.Value
import Minima.Interpreter.Environment
import Control.Monad.State
import Lens

%access public export

record InterpreterState i n where
  constructor MkInterpreterState
  _variables : Environment i n
  _io : i

variables : Lens (InterpreterState i n) (Environment i n)
variables = lens _variables setVariables where
  setVariables : Environment i n -> InterpreterState i n -> InterpreterState i n
  setVariables vs st = record { _variables = vs } st

io : Lens (InterpreterState i n) i
io = lens _io setIo where
  setIo : i -> InterpreterState i n -> InterpreterState i n
  setIo i st = record { _io = i } st

lastOf : List (Value i n) -> Value i n
lastOf [] = Success
lastOf vals@(_ :: _) = last vals

mutual
  inSequence : (Eq n, Show n) =>
               List (Expression () n) ->
               State (InterpreterState i n) (Either String (List (Value i n)))
  inSequence [] = pure $ Right []
  inSequence (x :: xs) = do (Right first) <- interpret x
                              | (Left error) => pure (Left error)
                            (Right rest) <- inSequence xs
                              | (Left error) => pure (Left error)
                            pure $ Right (first :: rest)

  invoke : (Eq n, Show n) => Value i n -> List (Value i n) -> State (InterpreterState i n) (Either String (Value i n))
  invoke (FunctionValue params body) args = do modify (variables ^%= enterScope)
                                               modify (variables ^%= defineAll (zip params args))
                                               (Right result) <- interpret body
                                                 | (Left error) => pure (Left error)
                                               modify (variables ^%= exitScope)
                                               pure $ Right result
  invoke (NativeFunction f) args = do interaction <- getL io <$> get
                                      let (Right (val, newInteraction)) = f interaction args
                                        | (Left error) => pure (Left error)
                                      modify (io ^= newInteraction)
                                      pure $ Right val
  invoke val _ = pure $ Left $ show val ++ " is not callable"

  interpret : (Eq n, Show n) => Expression () n -> State (InterpreterState i n) (Either String (Value i n))
  interpret (StringLiteral _ text) = pure $ pure $ StringValue text
  interpret (NumberLiteral _ number) = pure $ pure $ NumberValue number
  interpret (Variable _ name) = do value <- lookupValue name <$> getL variables <$> get
                                   pure value
  interpret (Definition _ name exp) = do (Right value) <- interpret exp
                                           | (Left error) => pure (Left error)
                                         modify (variables ^%= define name value)
                                         pure $ Right value
  interpret (Function _ args body) = pure $ pure $ FunctionValue args body
  interpret (Call _ fun args) = do (Right function) <- interpret fun
                                     | (Left error) => pure (Left error)
                                   (Right arguments) <- inSequence args
                                     | (Left error) => pure (Left error)
                                   invoke function arguments
  interpret (Group _ seq) = do modify (variables ^%= enterScope)
                               (Right results) <- inSequence seq
                                 | (Left error) => pure (Left error)
                               modify (variables ^%= exitScope)
                               pure $ Right (lastOf results)

runProgram : (Eq n, Show n) => List (Expression a n) -> State (InterpreterState i n) (Either String (Value i n))
runProgram expressions = do (Right results) <- inSequence (stripAnnotations <$> expressions)
                              | (Left error) => pure (Left error)
                            pure $ Right (lastOf results)
