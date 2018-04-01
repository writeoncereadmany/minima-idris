module Interpreter

import Minima.AST
import Minima.Annotations
import public Minima.Interpreter.Value
import public Minima.Interpreter.InterpreterState
import Lens

%access public export

ProgramState : Type -> Type
ProgramState i = Either String (InterpreterState i)

interpretStringLiteral : ProgramState i -> () -> String -> ProgramState i
interpretStringLiteral st _ val = (value ^= StringValue val) <$> st

interpretNumberLiteral : ProgramState i -> () -> Integer -> ProgramState i
interpretNumberLiteral st _ val = (value ^= NumberValue val) <$> st

lookupVariable : String -> InterpreterState i -> Either String (Value i)
lookupVariable name st = case lookup name (variables ^$ st) of
  Nothing => Left $ "Variable " ++ name ++ " is undefined"
  (Just val) => pure val

interpretVariable : ProgramState i -> () -> String -> ProgramState i
interpretVariable st _ name = do val <- st >>= lookupVariable name
                                 (value ^= val) <$> st

interpretDefinition : ProgramState i -> () -> String -> ProgramState i -> ProgramState i
interpretDefinition st _ name val = do toAssign <- getL value <$> val
                                       assigned <- (variables ^%= ((name, toAssign) ::)) <$> st
                                       pure $ value ^= Success $ assigned

interpretFunction : ProgramState i -> () -> List String -> Expression () -> ProgramState i
interpretFunction st _ args body = (value ^= FunctionValue args body) <$> st

interpretGroup : ProgramState i -> () -> List (ProgramState i) -> ProgramState i
interpretGroup st _ [] = (value ^= Success) <$> st
interpretGroup st _ exps@(x :: xs) = do oldVariables <- getL variables <$> st
                                        (variables ^= oldVariables) <$> (last exps)

allSucceeded : List (Either a b) -> Either a (List b)
allSucceeded xs = case lefts xs of
  [] => Right $ rights xs
  (x :: xs) => Left x

mutual
  call : InterpreterState i -> InterpreterState i -> List (InterpreterState i) -> ProgramState i
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

  interpretCall : ProgramState i -> () -> ProgramState i -> List (ProgramState i) -> ProgramState i
  interpretCall st' _ fun' args' = do st <- st'
                                      fun <- fun'
                                      args <- allSucceeded args'
                                      call st fun args

  interpreter : ExpressionSemantics () (ProgramState i)
  interpreter = MkExpressionSemantics
                  interpretStringLiteral
                  interpretNumberLiteral
                  interpretVariable
                  interpretDefinition
                  interpretFunction
                  interpretCall
                  interpretGroup

interpret : ProgramState i -> Expression a -> ProgramState i
interpret st expression = foldExpression interpreter st (stripAnnotations expression)

runProgram : InterpreterState i -> List (Expression a) -> ProgramState i
runProgram prelude = foldl interpret (pure prelude)
