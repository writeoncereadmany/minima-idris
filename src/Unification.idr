module Unification

import Bindings

%access public export

unify : List Bindings -> Bindings
unify [] = []
-- this is NOT a good implementation of unify!
unify xs = join xs
