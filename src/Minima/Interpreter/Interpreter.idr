module Interpreter

import Minima.AST
import Minima.Annotations
import Minima.Interpreter.Value
import Debug.Error

%language ElabReflection
%access public export

Environment : Type
Environment = List (String, Value)

record InterpreterState where
  constructor MkInterpreterState
  value : Value
  variables : Environment
  io : IO ()

Show InterpreterState where
  show x = show $ value x

interpretStringLiteral : InterpreterState -> () -> String -> InterpreterState
interpretStringLiteral st _ val = record { value = StringValue val } st

interpretNumberLiteral : InterpreterState -> () -> Integer -> InterpreterState
interpretNumberLiteral st _ val = record { value = NumberValue val } st

interpretVariable : InterpreterState -> () -> String -> InterpreterState
interpretVariable st _ name = case lookup name (variables st) of
  Nothing => error $ "Variable " ++ name ++ " is undefined"
  (Just val) => record { value = val } st

interpretDefinition : InterpreterState -> () -> String -> InterpreterState -> InterpreterState
interpretDefinition st _ name val = record { value = Success, variables $= ((name, value val) ::) } st

interpretFunction : InterpreterState -> () -> List String -> Expression () -> InterpreterState
interpretFunction st _ args body = record { value = FunctionValue args body } st

interpretGroup : InterpreterState -> () -> List InterpreterState -> InterpreterState
interpretGroup st _ [] = record { value = Success} st
interpretGroup st _ exps@(x :: xs) = record { variables = variables st } (last exps)

mutual
  interpretCall : InterpreterState -> () -> InterpreterState -> List InterpreterState -> InterpreterState
  interpretCall st _ fun args = case value fun of
    (FunctionValue params body) => if length args /= length params
      then error $ "Function called with wrong arity: expected " ++ show params ++ ", got " ++ show (value <$> args)
      else let bindings = zip params (value <$> args)
               funState = record { variables $= (bindings ++) } fun
            in foldExpression interpreter funState body
    notFun => error $ show notFun ++ " is not callable"

  interpreter : ExpressionSemantics () InterpreterState
  interpreter = MkExpressionSemantics
                  interpretStringLiteral
                  interpretNumberLiteral
                  interpretVariable
                  interpretDefinition
                  interpretFunction
                  interpretCall
                  interpretGroup

interpret : InterpreterState -> Expression a -> InterpreterState
interpret st expression = foldExpression interpreter st (stripAnnotations expression)

runProgram : InterpreterState -> List (Expression a) -> InterpreterState
runProgram = foldl interpret
