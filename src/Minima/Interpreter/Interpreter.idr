module Interpreter

import Minima.AST
import Minima.Annotations
import Minima.Interpreter.Value
import Debug.Error

%language ElabReflection
%access public export

Environment : Type -> Type
Environment i = List (String, (Value i))

record InterpreterState i where
  constructor MkInterpreterState
  value : Value i
  variables : Environment i
  io : i

Show (InterpreterState i) where
  show x = show $ value x

interpretStringLiteral : InterpreterState i -> () -> String -> InterpreterState i
interpretStringLiteral st _ val = record { value = StringValue val } st

interpretNumberLiteral : InterpreterState i -> () -> Integer -> InterpreterState i
interpretNumberLiteral st _ val = record { value = NumberValue val } st

interpretVariable : InterpreterState i -> () -> String -> InterpreterState i
interpretVariable st _ name = case lookup name (variables st) of
  Nothing => error $ "Variable " ++ name ++ " is undefined"
  (Just val) => record { value = val } st

interpretDefinition : InterpreterState i -> () -> String -> InterpreterState i -> InterpreterState i
interpretDefinition st _ name val = record { value = Success, variables $= ((name, value val) ::) } st

interpretFunction : InterpreterState i -> () -> List String -> Expression () -> InterpreterState i
interpretFunction st _ args body = record { value = FunctionValue args body } st

interpretGroup : InterpreterState i -> () -> List (InterpreterState i) -> InterpreterState i
interpretGroup st _ [] = record { value = Success} st
interpretGroup st _ exps@(x :: xs) = record { variables = variables st } (last exps)

mutual
  interpretCall : InterpreterState i -> () -> InterpreterState i -> List (InterpreterState i) -> InterpreterState i
  interpretCall st _ fun args = case value fun of
    (NativeFunction f) => let oldIo = io $ last (fun :: args)
                              (val, newIo) = f oldIo (value <$> args)
                           in record { value = val, io = newIo } st
    (FunctionValue params body) => if length args /= length params
      then error $ "Function called with wrong arity: expected " ++ show params ++ ", got " ++ show (value <$> args)
      else let bindings = zip params (value <$> args)
               funState = record { variables $= (bindings ++) } fun
            in foldExpression interpreter funState body
    notFun => error $ show notFun ++ " is not callable"

  interpreter : ExpressionSemantics () (InterpreterState i)
  interpreter = MkExpressionSemantics
                  interpretStringLiteral
                  interpretNumberLiteral
                  interpretVariable
                  interpretDefinition
                  interpretFunction
                  interpretCall
                  interpretGroup

interpret : InterpreterState i -> Expression a -> InterpreterState i
interpret st expression = foldExpression interpreter st (stripAnnotations expression)

runProgram : InterpreterState i -> List (Expression a) -> InterpreterState i
runProgram = foldl interpret
