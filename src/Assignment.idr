module Assignment

import Bindings
import MinimaTypes
import TypeErrors
import Unification

%access public export

data AssOp = Assignment DeBruijnIndex DeBruijnIndex
infixl 8 ->?
(->?) : DeBruijnIndex -> DeBruijnIndex -> AssOp
(->?) = Assignment

mutual
  WithBindings AssOp (Either TypeErrors Bindings) where
    (|=>) bindings (Assignment src tgt) = assignTo bindings src tgt

  assignUnion : (bindings : Bindings) -> (tgts : List DeBruijnIndex) -> (src : DeBruijnIndex) -> Either TypeErrors Bindings
  assignUnion bindings tgts src = case assignTo bindings src <$> tgts of
     [] => Left ["Cannot assign to an empty union"]
     xs => case rights xs of
           [] => Left ["No suitable target type for " ++ show src ++ " in union " ++ show tgts]
           (x :: xs) => Right x

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
            [] => (unifyBindings $ bindings :: rights assignments) |=> sret ->? tret
            typeErrors => Left $ join typeErrors
      (Union srcs, Union tgts) => case assignUnion bindings tgts <$> srcs of
         [] => Left ["Empty union"]
         xs => case lefts xs of
            [] => Right bindings
            typeErrors => Left $ join typeErrors
      (_, Union tgts) => assignUnion bindings tgts src
      (stype, ttype) => Left ["Cannot assign " ++ show stype ++ " to " ++ show ttype]
