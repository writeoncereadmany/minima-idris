module Unification

import MinimaTypes
import Bindings
import TypeErrors

%access public export

data UnificationOp = Unify MinimaType MinimaType

infixl 8 |?
(|?) : MinimaType -> MinimaType -> UnificationOp
(|?) = Unify

mutual
  WithBindings UnificationOp (Either TypeErrors MinimaType) where
    (|=>) bindings (Unify a b) = unify bindings a b

  unify : Bindings -> MinimaType -> MinimaType -> Either TypeErrors MinimaType
  -- shortcut the unification if they're the same name
  unify b nx@(Named x) ny@(Named y) = if x == y
    then Right nx
    else b |=> lookupType x b |? ny
  unify b (Named x) y = b |=> lookupType x b |? y
  unify b x (Named y) = b |=> x |? lookupType y b

  unify b (Unbound _) (Unbound _) = Left ["Trying to unify two unbound types. Where did they come from?"]
  unify b (Unbound _) y = Right y
  unify b x (Unbound _) = Right x

  unify b (Union xs) (Union ys) = Right $ Union $ sort $ union xs ys
  unify b x@(Union _) y = b |=> x |? Union [y]
  unify b x y@(Union _) = b |=> Union [x] |? y

  unify b dx@(Data x) dy@(Data y) = if x == y
    then Right dx
    else Right $ Union [dx, dy]

  unify b (Function args returns) y = ?unify_rhs_3
