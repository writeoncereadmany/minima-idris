module Interpreter

import Minima.AST
import Minima.Annotators.Annotations
import Minima.Interpreter.Value
import Minima.Interpreter.Environment
import Minima.Interpreter.InterpreterState
import Control.Monad.State
import Lens

%access public export

lastOf : List (Value i n) -> Value i n
lastOf [] = Success
lastOf vals@(_ :: _) = last vals

always : a -> State _ (Either _ a)
always = pure . pure

mutual
  inSequence : (Eq n, Show n) =>
               List (Expression () n) ->
               State (InterpreterState i n) (Either String (List (Value i n)))
  inSequence [] = pure $ Right []
  inSequence (x :: xs) = do
       (Right first) <- interpret x | (Left error) => pure (Left error)
       (Right rest) <- inSequence xs | (Left error) => pure (Left error)
       pure $ Right (first :: rest)

  invoke : (Eq n, Show n) => Value i n -> List (Value i n) -> State (InterpreterState i n) (Either String (Value i n))
  invoke (FunctionValue params body) args = do
       modify (variables ^%= enterScope)
       modify (variables ^%= defineAll (zip params args))
       (Right result) <- interpret body | (Left error) => pure (Left error)
       modify (variables ^%= exitScope)
       pure $ Right result
  invoke (NativeFunction f) args = do
       interaction <- getL io <$> get
       let (Right (val, newInteraction)) = f interaction args | (Left error) => pure (Left error)
       modify (io ^= newInteraction)
       pure $ Right val
  invoke val _ = pure $ Left $ show val ++ " is not callable"

  interpret : (Eq n, Show n) => Expression () n -> State (InterpreterState i n) (Either String (Value i n))
  interpret (StringLiteral _ text) = always $ StringValue text
  interpret (NumberLiteral _ number) = always $ NumberValue number
  interpret (Function _ args body) = always $ FunctionValue args body
  interpret (Variable _ name) = do
       value <- lookupValue name <$> getL variables <$> get
       pure $ (maybeToEither (show name ++ " is undefined")) value
  interpret (Definition _ name exp) = do
       (Right value) <- interpret exp | (Left error) => pure (Left error)
       modify (variables ^%= define name value)
       pure $ Right Success
  interpret (Call _ fun args) = do
       (Right function) <- interpret fun | (Left error) => pure (Left error)
       (Right arguments) <- inSequence args | (Left error) => pure (Left error)
       invoke function arguments
  interpret (Group _ seq) = do
       modify (variables ^%= enterScope)
       (Right results) <- inSequence seq | (Left error) => pure (Left error)
       modify (variables ^%= exitScope)
       pure $ Right (lastOf results)

runProgram : (Eq n, Show n) => List (Expression a n) -> State (InterpreterState i n) (Either String (Value i n))
runProgram expressions =
  do (Right results) <- inSequence (stripAnnotations <$> expressions) | (Left error) => pure (Left error)
     pure $ Right (lastOf results)
