module MinimaTypes2

%access public export

-- refers to a location in code, where the type is defined.
-- Used for uniquely referencing concepts - names get converted to these quite early
DeBruijnIndex : Type
DeBruijnIndex = (Integer, Integer)

-- by having types refer to other types, type synthesis is no biggie
data MinimaType2 : Type where
  Bound : (refersTo : DeBruijnIndex) -> MinimaType2
  Primitive : (initialDefinition : DeBruijnIndex) -> MinimaType2
  Function : (args : List MinimaType2) -> (returns : MinimaType2) -> MinimaType2
  Union : (forms : List MinimaType2) -> MinimaType2
  Unbound : MinimaType2

Show MinimaType2 where
  show (Bound refersTo) = "Bound " ++ show refersTo
  show (Primitive initialDefinition) = "Primitive " ++ show initialDefinition
  show (Function args returns) = show args ++ show returns
  show (Union forms) = "Union " ++ show forms
  show Unbound = "Unbound"

mutual
  eq : MinimaType2 -> MinimaType2 -> Bool
  eq (Bound a) (Bound b) = a == b
  eq (Primitive a) (Primitive b) = a == b
  eq (Function args1 rets1) (Function args2 rets2) = allEq args1 args2 && eq rets1 rets2
  eq (Union as) (Union bs) = allEq as bs
  eq Unbound Unbound = True
  eq _ _ = False

  allEq : List MinimaType2 -> List MinimaType2 -> Bool
  allEq [] [] = True
  allEq [] __ = False
  allEq __ [] = False
  allEq (x :: xs) (y :: ys) = eq x y && allEq xs ys

Eq MinimaType2 where
  (==) = eq
