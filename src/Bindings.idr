module Bindings

import MinimaTypes

%access public export

Bindings : Type
Bindings = List (DeBruijnIndex, MinimaType2)

lookupType : DeBruijnIndex -> Bindings -> MinimaType2
lookupType i b = case lookup i b of
    Nothing => Unbound
    (Just type) => type

infixl 4 |=>
interface WithBindings a b | a where
  (|=>) : Bindings -> a -> b
