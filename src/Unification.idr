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
