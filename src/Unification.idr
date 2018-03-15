module Unification

import Bindings
import MinimaTypes

%access public export

unifyBindings : List Bindings -> Bindings
unifyBindings [] = []
-- this is NOT a good implementation of unify!
unifyBindings xs = join xs

data UnionOp = UnionOperation DeBruijnIndex DeBruijnIndex
infixl 8 |?
(|?) : DeBruijnIndex -> DeBruijnIndex -> UnionOp
(|?) = UnionOperation

mutual
  WithBindings UnionOp MinimaType where
    (|=>) bindings (UnionOperation x y) = unifyBinding bindings x y

  unifyBinding : Bindings -> DeBruijnIndex -> DeBruijnIndex -> MinimaType
  unifyBinding bindings a b = case (lookupType a bindings, lookupType b bindings) of
     (Union xs, Union ys)       => Union (sort $ union xs ys)
     (Union xs, _)              => Union (sort $ union xs [b])
     (_, Union ys)              => Union (sort $ union [a] ys)
     (Primitive x, Primitive y) => if x == y then Primitive x else Union (sort [a, b])
     (_, _) => Unbound
