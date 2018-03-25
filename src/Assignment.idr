module Assignment

import Bindings
import MinimaTypes
import TypeErrors

%access public export

data AssOp2 = Assignment MinimaType MinimaType
infixl 8 ->?
(->?) : MinimaType -> MinimaType -> AssOp2
(->?) = Assignment

data JustFunction = JFunction (List MinimaType) MinimaType

mutual
  WithBindings AssOp2 (Either TypeErrors Bindings) where
    (|=>) bindings (Assignment src tgt) = assignTo bindings src tgt

  assignArgs : Bindings -> List MinimaType -> List MinimaType -> Either TypeErrors Bindings
  assignArgs b srcargs tgtargs = if (length srcargs /= length tgtargs)
         then Left ["Arity mismatch."]
         else let assignments = zipWith (assignTo b) tgtargs srcargs
           in case lefts $ assignments of
              [] => Right $ mergeAll $ rights assignments
              typeErrors => Left $ join typeErrors

  assignToFunction : Bindings -> JustFunction -> JustFunction -> Either TypeErrors Bindings
  assignToFunction b (JFunction srcargs srcret) (JFunction tgtargs tgtret) =
       do newBindings <- assignArgs b srcargs tgtargs
          mergeBindings b newBindings |=> srcret ->? tgtret

  assignToUnion : Bindings -> MinimaType -> List MinimaType -> Either TypeErrors Bindings
  assignToUnion b src tgts = let assignments = assignTo b src <$> tgts
                              in case rights assignments of
                                 []       => Left $ join $ lefts assignments
                                 bindings => Right $ foldl mergeBindings [] bindings

  assignUnionTo : Bindings -> List MinimaType -> MinimaType -> Either TypeErrors Bindings
  assignUnionTo b srcs tgt = let assignments = (flip (assignTo b) tgt) <$> srcs
                              in case lefts assignments of
                                 [] => Right $ mergeAll (rights assignments)
                                 typeErrors => Left $ join typeErrors

  assignTo : Bindings -> MinimaType -> MinimaType -> Either TypeErrors Bindings
  assignTo bindings a (Unbound b) = Right [(b, a)]
  assignTo bindings (Unbound a) tgt = case lookupType a bindings of
    (Unbound _) => Left ["Cannot assign unbound to " ++ show tgt]
    other => bindings |=> other ->? tgt

  assignTo bindings (Bound a) b = bindings |=> lookupType a bindings ->? b
  assignTo bindings a (Bound b) = bindings |=> a ->? lookupType b bindings

  assignTo bindings (Union srcs) tgt = assignUnionTo bindings srcs tgt
  assignTo bindings src (Union tgts) = assignToUnion bindings src tgts

  assignTo bindings (Data src) (Data tgt) = if src == tgt
    then Right []
    else Left ["Cannot assign " ++ show src ++ " to " ++ show tgt]

  assignTo bindings (Function srcargs srcret) (Function tgtargs tgtret) =
    assignToFunction bindings (JFunction srcargs srcret) (JFunction tgtargs tgtret)

  assignTo bindings a b = Left ["Cannot assign " ++ show a ++ " to " ++ show b]
