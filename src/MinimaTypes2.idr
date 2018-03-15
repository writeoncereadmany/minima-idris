module MinimaTypes2

%access public export

DeBruijnIndex : Type
DeBruijnIndex = (Integer, Integer)

data MinimaType2 : Type where
  Unbound : MinimaType2
  Primitive : (location : DeBruijnIndex) -> MinimaType2
  Function : (args : List DeBruijnIndex) -> (returnValue : DeBruijnIndex) -> MinimaType2

Show MinimaType2 where
  show Unbound = "Unbound"
  show (Primitive location) = show location
  show (Function args returnType) = show args ++ " => " ++ show returnType

Eq MinimaType2 where
  (==) Unbound Unbound = True
  (==) (Primitive a) (Primitive b) = a == b
  (==) (Function args1 ret1) (Function args2 ret2) = args1 == args2 && ret1 == ret2
  (==) _ _ = False

Bindings : Type
Bindings = List (DeBruijnIndex, MinimaType2)

lookupType : DeBruijnIndex -> Bindings -> MinimaType2
lookupType i b = case lookup i b of
    Nothing => Unbound
    (Just type) => type

TypeError : Type
TypeError = String

TypeErrors : Type
TypeErrors = List TypeError

infixl 4 |=>
interface WithBindings a b | a where
  (|=>) : Bindings -> a -> b

data AssOp = Assignment DeBruijnIndex DeBruijnIndex
infixl 8 ->?
(->?) : DeBruijnIndex -> DeBruijnIndex -> AssOp
(->?) = Assignment

unify : List Bindings -> Bindings
unify [] = []
-- this is NOT a good implementation of unify!
unify xs = join xs

mutual
  WithBindings AssOp (Either TypeErrors Bindings) where
    (|=>) bindings (Assignment src tgt) = assignTo bindings src tgt

  assignTo : Bindings -> (src : DeBruijnIndex) -> (tgt : DeBruijnIndex) -> Either TypeErrors Bindings
  assignTo bindings src tgt = if src == tgt
    then Right bindings
    else case (lookupType src bindings, lookupType tgt bindings) of
      (Unbound, _) => Left ["Source type " ++ show src ++ " must be bound"]
      (type, Unbound) => Right $ (tgt, type) :: bindings
      (Primitive s, Primitive t) => if s == t
         then Right bindings
         else Left ["Cannot assign " ++ show s ++ " to " ++ show t]
      (Function sargs sret, Function targs tret) => case zipWith (assignTo bindings) targs sargs of
         assignments => case lefts assignments of
            [] => (unify $ bindings :: rights assignments) |=> sret ->? tret
            typeErrors => Left $ join typeErrors
      (stype, ttype) => Left ["Cannot assign " ++ show stype ++ " to " ++ show ttype]
