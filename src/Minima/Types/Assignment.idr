module Assignment

import Minima.Types.Bindings
import Minima.Types.MinimaTypes
import Minima.Types.TypeErrors
import Minima.Types.Unification

%access public export

data AssOp = Assignment MinimaType MinimaType
infixl 8 ->?
(->?) : MinimaType -> MinimaType -> AssOp
(->?) = Assignment

data JustFunction = JFunction (List MinimaType) MinimaType

mutual
  WithBindings AssOp (Either TypeErrors Bindings) where
    (|=>) bindings (Assignment src tgt) = assignTo bindings src tgt

  assignArgs : Bindings -> List MinimaType -> List MinimaType -> Either TypeErrors Bindings
  assignArgs b srcargs tgtargs = if (length srcargs /= length tgtargs)
         then Left ["Arity mismatch."]
         else let assignments = zipWith (assignTo b) tgtargs srcargs
           in case lefts $ assignments of
              [] => Right $ unifyAll $ rights assignments
              typeErrors => Left $ join typeErrors

  assignToFunction : Bindings -> JustFunction -> JustFunction -> Either TypeErrors Bindings
  assignToFunction b (JFunction srcargs srcret) (JFunction tgtargs tgtret) =
       do newBindings <- assignArgs b srcargs tgtargs
          unifyBindings b newBindings |=> srcret ->? tgtret

  assignToUnion : Bindings -> MinimaType -> List MinimaType -> Either TypeErrors Bindings
  assignToUnion b src tgts = let assignments = assignTo b src <$> tgts
                              in case rights assignments of
                                 []       => Left $ join $ lefts assignments
                                 bindings => Right $ unifyAll bindings

  assignUnionTo : Bindings -> List MinimaType -> MinimaType -> Either TypeErrors Bindings
  assignUnionTo b srcs tgt = let assignments = (flip (assignTo b) tgt) <$> srcs
                              in case lefts assignments of
                                 [] => Right $ unifyAll (rights assignments)
                                 typeErrors => Left $ join typeErrors

  assignTo : Bindings -> MinimaType -> MinimaType -> Either TypeErrors Bindings

  assignTo bindings a (Unbound b) = Right [(b, a)]
  assignTo bindings (Unbound a) tgt = Left ["Cannot assign unbound to " ++ show tgt]

  assignTo bindings (Named a) b = bindings |=> lookupType a bindings ->? b
  assignTo bindings a (Named b) = bindings |=> a ->? lookupType b bindings

  assignTo bindings _ Anything = Right bindings
  assignTo bindings Anything b = Left ["Cannot assign Anything to " ++ show b]

  assignTo bindings Nothing _ = Right bindings
  assignTo bindings a Nothing = Left ["Cannot assign " ++ show a ++ " to Nothing"]

  assignTo bindings (Union srcs) tgt = assignUnionTo bindings srcs tgt
  assignTo bindings src (Union tgts) = assignToUnion bindings src tgts

  assignTo bindings (Data src) (Data tgt) = if src == tgt
    then Right []
    else Left ["Cannot assign " ++ show src ++ " to " ++ show tgt]

  assignTo bindings (Function srcargs srcret) (Function tgtargs tgtret) =
    assignToFunction bindings (JFunction srcargs srcret) (JFunction tgtargs tgtret)

  assignTo bindings a b = Left ["Cannot assign " ++ show a ++ " to " ++ show b]
