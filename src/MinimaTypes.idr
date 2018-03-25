module MinimaTypes

%access public export

-- refers to a location in code, where the type is defined.
-- Used for uniquely referencing concepts - names get converted to these quite early
DeBruijnIndex : Type
DeBruijnIndex = (Integer, Integer)

-- by having types refer to other types, type synthesis is no biggie
data MinimaType : Type where
  Anything : MinimaType
  Nothing : MinimaType
  Named : (introduction : DeBruijnIndex) -> MinimaType
  Data : (initialDefinition : DeBruijnIndex) -> MinimaType
  Function : (args : List MinimaType) -> (returns : MinimaType) -> MinimaType
  Union : (forms : List MinimaType) -> MinimaType
  Unbound : (introduction : DeBruijnIndex) -> MinimaType

Show MinimaType where
  show Anything = "Anything"
  show Nothing = "Nothing"
  show (Named refersTo) = "Named " ++ show refersTo
  show (Data initialDefinition) = "Data " ++ show initialDefinition
  show (Function args returns) = show args ++ " => " ++ show returns
  show (Union forms) = "Union " ++ show forms
  show (Unbound introduction) = "Unbound " ++ show introduction

mutual
  eq : MinimaType -> MinimaType -> Bool
  eq Anything Anything = True
  eq Nothing Nothing = True
  eq (Named a) (Named b) = a == b
  eq (Data a) (Data b) = a == b
  eq (Function args1 rets1) (Function args2 rets2) = allEq args1 args2 && eq rets1 rets2
  eq (Union as) (Union bs) = allEq as bs
  eq (Unbound a) (Unbound b) = a == b
  eq _ _ = False

  allEq : List MinimaType -> List MinimaType -> Bool
  allEq [] [] = True
  allEq [] __ = False
  allEq __ [] = False
  allEq (x :: xs) (y :: ys) = eq x y && allEq xs ys

Eq MinimaType where
  (==) = eq

constructorIndex : MinimaType -> Int
constructorIndex Anything = 0
constructorIndex Nothing = 1
constructorIndex (Named _) = 2
constructorIndex (Data _) = 3
constructorIndex (Function _ _) = 4
constructorIndex (Union _) = 5
constructorIndex (Unbound _) = 6

mutual
  compareLists : List MinimaType -> List MinimaType -> Ordering
  compareLists [] [] = EQ
  compareLists [] (x :: xs) = LT
  compareLists (x :: xs) [] = GT
  compareLists (x :: xs) (y :: ys) = case compare x y of
    EQ => compareLists xs ys
    diff => diff

  Ord MinimaType where
    compare Anything Anything = EQ
    compare Nothing Nothing = EQ
    compare (Named a) (Named b) = compare a b
    compare (Data a) (Data b) = compare a b
    compare (Function argsa reta) (Function argsb retb) = case compareLists argsa argsb of
      EQ => compare reta retb
      diff => diff
    compare (Union as) (Union bs) = compareLists as bs
    compare (Unbound a) (Unbound b) = compare a b
    compare a b = compare (constructorIndex a) (constructorIndex b)
