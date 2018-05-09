module Value

import Minima.AST

%access public export

mutual
  Implementation : Type -> Type -> Type -> Type
  Implementation a i n = i -> List (Value a i n) -> Either String (Value a i n, i)

  data Value a i n = NumberValue Integer
                  | StringValue String
                  | Success
                  | FunctionValue (List n) (Expression a n)
                  | NativeFunction (Implementation a i n)

(Eq n, Eq a) => Eq (Value a i n) where
  (==) (NumberValue x) (NumberValue y) = x == y
  (==) (StringValue x) (StringValue y) = x == y
  (==) Success Success = True
  (==) (FunctionValue args1 rets1) (FunctionValue args2 rets2) = args1 == args2 && rets1 == rets2
  (==) _ _ = False

Show n => Show (Value a i n) where
  show (NumberValue x) = show x
  show (StringValue x) = x
  show Success = "Success"
  show (FunctionValue xs x) = show xs ++ " => " ++ show x
  show (NativeFunction _) = "Native Function"
