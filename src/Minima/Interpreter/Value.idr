module Value

import Minima.AST

%access public export

mutual
  Implementation : Type -> Type -> Type
  Implementation i n = i -> List (Value i n) -> Either String (Value i n, i)

  data Value i n = NumberValue Integer
                 | StringValue String
                 | Success
                 | FunctionValue (List n) (Expression () n)
                 | NativeFunction (Implementation i n)

Eq n => Eq (Value i n) where
  (==) (NumberValue x) (NumberValue y) = x == y
  (==) (StringValue x) (StringValue y) = x == y
  (==) Success Success = True
  (==) (FunctionValue args1 rets1) (FunctionValue args2 rets2) = args1 == args2 && rets1 == rets2
  (==) _ _ = False

Show n => Show (Value i n) where
  show (NumberValue x) = show x
  show (StringValue x) = x
  show Success = "Success"
  show (FunctionValue xs x) = show xs ++ " => " ++ show x
  show (NativeFunction _) = "Native Function"
