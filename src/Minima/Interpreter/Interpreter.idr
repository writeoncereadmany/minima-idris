module Interpreter

import Minima.AST
import Minima.Annotators.Annotations
import Minima.Interpreter.Value
import Minima.Interpreter.DependentEnvironment
import Minima.Interpreter.InterpreterState
import Control.ST
import Lens

%access public export

lastOf : List (Value a i n) -> Value a i n
lastOf [] = Success
lastOf vals@(_ :: _) = last vals

mutual
  inSequence : (Eq n, Show n)
            => (env : Var)
            -> List (Expression a n)
            -> ST (Either String) (List (Value a i n)) [ env ::: State (InterpreterState (S d) a i n) ]
  inSequence env [] = pure $ []
  inSequence env (x :: xs) = pure (!(interpret env x) :: !(inSequence env xs))

  invoke : (Eq n, Show n)
        => (env : Var)
        -> Value a i n
        -> List (Value a i n)
        -> ST (Either String) (Value a i n) [env ::: State (InterpreterState (S d) a i n) ]
  invoke env (FunctionValue params body) args = do
       update env enter
       update env (variables ^%= defineAll (zip params args))
       result <- interpret env body
       update env exit
       pure result
  invoke env (NativeFunction f) args = do
       env' <- read env
       let interaction = getL io env'
       (val, newInteraction) <- lift $ f interaction args
       update env (io ^= newInteraction)
       pure val
  invoke env val _ = lift $ Left $ show val ++ " is not callable"

  interpret : (Eq n, Show n)
           => (env : Var)
           -> Expression a n
           -> ST (Either String) (Value a i n) [env ::: State (InterpreterState (S d) a i n)]
  interpret env (StringLiteral _ text) = pure $ StringValue text
  interpret env (NumberLiteral _ number) = pure $ NumberValue number
  interpret env (Function _ args body) = pure $ FunctionValue args body
  interpret env (Variable _ name) = do
       env' <- read env
       let vars = getL variables env'
       let value = lookupValue name vars
       value <- lift $ (maybeToEither (show name ++ " is undefined")) $ lookupValue name vars
       pure value
  interpret env (Definition _ name exp) = do
       value <- interpret env exp
       update env (variables ^%= define name value)
       pure Success
  interpret env (Call _ fun args) = do
       function <- interpret env fun
       arguments <- inSequence env args
       invoke env function arguments
  interpret env (Group _ seq) = do
       update env enter
       results <- inSequence env seq
       update env exit
       pure $ lastOf results

runInterpreter' : (Show i, Eq i)
               => InterpreterState (S d) as io i
               -> Expression as i
               -> ST (Either String) (InterpreterState (S d) as io i, Value as io i) []
runInterpreter' prelude exp = do
  env <- new prelude
  update env enter
  result <- interpret env exp
  update env exit
  finalState <- read env
  delete env
  pure (finalState, result)

runInterpreter : (Show i, Eq i)
              => InterpreterState (S d) as io i
              -> Expression as i
              -> Either String (InterpreterState (S d) as io i, Value as io i)
runInterpreter prelude exp = run $ runInterpreter' prelude exp

evalExp : (Show i, Eq i)
       => InterpreterState (S d) as io i
       -> Expression as i
       -> Either String (Value as io i)
evalExp prelude exp = do
  (state, value) <- runInterpreter prelude exp
  pure value

outputsFrom : (Show i, Eq i)
          => InterpreterState (S d) as io i
          -> Expression as i
          -> Either String (InterpreterState (S d) as io i)
outputsFrom prelude exp = do
  (state, value) <- runInterpreter prelude exp
  pure state
