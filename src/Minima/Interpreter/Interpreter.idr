module Interpreter

import Minima.AST
import Minima.Annotations
import Minima.Whatever
import public Minima.Interpreter.Value
import public Minima.Interpreter.InterpreterState
import Lens

%access public export

ProgramState : Type -> Type -> Type
ProgramState i n = Either String (InterpreterState i n)

interpretStringLiteral : ProgramState i n -> () -> String -> ProgramState i n
interpretStringLiteral st _ val = (value ^= StringValue val) <$> st

interpretNumberLiteral : ProgramState i n -> () -> Integer -> ProgramState i n
interpretNumberLiteral st _ val = (value ^= NumberValue val) <$> st

lookupVariable : (Eq n, Show n) => n -> InterpreterState i n -> Either String (Value i n)
lookupVariable name st = case lookup name (variables ^$ st) of
  Nothing => Left $ "Variable " ++ show name ++ " is undefined"
  (Just val) => pure val

interpretVariable : (Eq n, Show n) => ProgramState i n -> () -> n -> ProgramState i n
interpretVariable st _ name = do val <- st >>= lookupVariable name
                                 (value ^= val) <$> st

interpretDefinition : ProgramState i n -> () -> n -> ProgramState i n -> ProgramState i n
interpretDefinition st _ name val = do toAssign <- getL value <$> val
                                       assigned <- (variables ^%= ((name, toAssign) ::)) <$> st
                                       pure $ value ^= Success $ assigned

interpretFunction : ProgramState i n -> () -> List n -> Expression () n -> ProgramState i n
interpretFunction st _ args body = (value ^= FunctionValue args body) <$> st

interpretGroup : ProgramState i n -> () -> List (ProgramState i n) -> ProgramState i n
interpretGroup st _ [] = (value ^= Success) <$> st
interpretGroup st _ exps@(x :: xs) = do oldVariables <- getL variables <$> st
                                        (variables ^= oldVariables) <$> (last exps)

allSucceeded : List (Either a b) -> Either a (List b)
allSucceeded xs = case lefts xs of
  [] => Right $ rights xs
  (x :: xs) => Left x

mutual
  call : (Eq n, Show n) => InterpreterState i n -> InterpreterState i n -> List (InterpreterState i n) -> ProgramState i n
  call st fun args = case value ^$ fun of
    (NativeFunction f) => do let oldIo = io ^$ last (fun :: args)
                             (val, newIo) <- f oldIo (getL value <$> args)
                             pure $ value ^= val $ io ^= newIo $ st
    (FunctionValue params body) => if length args /= length params
      then Left $ "Function called with wrong arity: expected " ++ show params ++ ", got " ++ show (getL value <$> args)
      else let bindings = zip params (getL value <$> args)
               bound = variables ^%= (bindings ++) $ last (fun :: args)
            in foldExpression interpreter (pure bound) body
    notFun => Left $ show notFun ++ " is not callable"

  interpretCall : (Eq n, Show n) => ProgramState i n -> () -> ProgramState i n -> List (ProgramState i n) -> ProgramState i n
  interpretCall st' _ fun' args' = do st <- st'
                                      fun <- fun'
                                      args <- allSucceeded args'
                                      call st fun args

  interpreter : (Eq n, Show n) => ExpressionSemantics () n (ProgramState i n)
  interpreter = MkExpressionSemantics
                  interpretStringLiteral
                  interpretNumberLiteral
                  interpretVariable
                  interpretDefinition
                  interpretFunction
                  interpretCall
                  interpretGroup

interpret : (Eq n, Show n, Whatever n) => ProgramState i n -> Expression a n -> ProgramState i n
interpret st expression = foldExpression interpreter st (stripAnnotations expression)

runProgram : (Eq n, Show n, Whatever n) => InterpreterState i n -> List (Expression a n) -> ProgramState i n
runProgram prelude = foldl interpret (pure prelude)
