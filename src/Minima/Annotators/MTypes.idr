module MTypes

import Minima.Annotators.UniqueIndexer

%access public export

data MType = MString
           | MNumber
           | MSuccess
           | MFunction (List MType) MType
           | MUnbound Index

data MTypeError = MkTypeError String

mutual
  Eq MType where
    (==) MString MString = True
    (==) MNumber MNumber = True
    (==) MSuccess MSuccess = True
    (==) (MUnbound x) (MUnbound y) = x == y
    (==) (MFunction args1 ret1) (MFunction args2 ret2) = allEq args1 args2 && ret1 == ret2
    (==) _ _ = False

  allEq : List MType -> List MType -> Bool
  allEq [] [] = True
  allEq [] (_ :: _) = False
  allEq (_ :: _) [] = False
  allEq (x :: xs) (y :: ys) = x == y && allEq xs ys

Eq MTypeError where
  (==) (MkTypeError a) (MkTypeError b) = a == b

Show MType where
  show MString = "String"
  show MNumber = "Number"
  show MSuccess = "Success"
  show (MUnbound x) = "Unbound " ++ show x
  show (MFunction xs x) = show xs ++ " -> " ++ show x

Show MTypeError where
  show (MkTypeError x) = "Type Error: " ++ x
