module Unification

import Bindings
import MinimaTypes

%access public export

unifyBindings : List Bindings -> Bindings
unifyBindings [] = []
-- this is NOT a good implementation of unify!
unifyBindings xs = join xs

unifyTypes : Bindings -> MinimaType -> MinimaType
