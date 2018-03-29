module Value

import Minima.AST

%access public export

data Value = NumberValue Integer
           | StringValue String
           | Success
           | FunctionValue (List String) (Expression ())

Eq Value where
  (==) (NumberValue x) (NumberValue y) = x == y
  (==) (StringValue x) (StringValue y) = x == y
  (==) Success Success = True
  (==) (FunctionValue args1 rets1) (FunctionValue args2 rets2) = args1 == args2 && rets1 == rets2
  (==) _ _ = False

Show Value where
  show (NumberValue x) = show x
  show (StringValue x) = x
  show Success = "Success"
  show (FunctionValue xs x) = show xs ++ " => " ++ show x
