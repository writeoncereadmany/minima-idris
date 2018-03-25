module Assignment2

import Bindings2
import MinimaTypes2
import TypeErrors
import Unification

%access public export

data AssOp2 = Assignment MinimaType2 MinimaType2
infixl 8 ->?
(->?) : MinimaType2 -> MinimaType2 -> AssOp2
(->?) = Assignment

data JustFunction = JFunction (List MinimaType2) MinimaType2

mutual
  WithBindings2 AssOp2 (Either TypeErrors Bindings2) where
    (|=>) bindings (Assignment src tgt) = assignTo bindings src tgt

  assignToFunction : Bindings2 -> JustFunction -> JustFunction -> Either TypeErrors Bindings2
  assignToFunction b (JFunction srcargs srcret) (JFunction tgtargs tgtret) =
       if (length srcargs /= length tgtargs)
       then Left ["Arity mismatch."]
       else let assignments = zipWith (assignTo b) tgtargs srcargs
             in case lefts assignments of
               [] => mergeAll (b :: rights assignments) |=> srcret ->? tgtret
               typeErrors => Left $ join typeErrors

  assignToUnion : Bindings2 -> MinimaType2 -> List MinimaType2 -> Either TypeErrors Bindings2
  assignToUnion b src tgts = let assignments = assignTo b src <$> tgts
                              in case rights assignments of
                                 []       => Left $ join $ lefts assignments
                                 bindings => Right $ foldl mergeBindings [] bindings

  assignUnionTo : Bindings2 -> List MinimaType2 -> MinimaType2 -> Either TypeErrors Bindings2
  assignUnionTo b srcs tgt = let assignments = (flip (assignTo b) tgt) <$> srcs
                              in case lefts assignments of
                                 [] => Right $ mergeAll (rights assignments)
                                 typeErrors => Left $ join typeErrors

  assignTo : Bindings2 -> MinimaType2 -> MinimaType2 -> Either TypeErrors Bindings2
  assignTo bindings a (Unbound b) = Right [(b, a)]
  assignTo bindings (Unbound a) tgt = Left ["Cannot assign unbound to " ++ show tgt]

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
