module MinimaTypes

%access public export

DeBruijnIndex : Type
DeBruijnIndex = (Integer, Integer)

data MinimaType2 : Type where
  Unbound : MinimaType2
  Primitive : (location : DeBruijnIndex) -> MinimaType2
  Function : (args : List DeBruijnIndex) -> (returnValue : DeBruijnIndex) -> MinimaType2
  Union : (forms : List DeBruijnIndex) -> MinimaType2

Show MinimaType2 where
  show Unbound = "Unbound"
  show (Primitive location) = show location
  show (Function args returnType) = show args ++ " => " ++ show returnType
  show (Union forms) = show forms

Eq MinimaType2 where
  (==) Unbound Unbound = True
  (==) (Primitive a) (Primitive b) = a == b
  (==) (Function args1 ret1) (Function args2 ret2) = args1 == args2 && ret1 == ret2
  (==) (Union a) (Union b) = a == b
  (==) _ _ = False
