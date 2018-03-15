module Bindings

import MinimaTypes

%access public export

Bindings : Type
Bindings = List (DeBruijnIndex, MinimaType)

lookupType : DeBruijnIndex -> Bindings -> MinimaType
lookupType i b = case lookup i b of
    Nothing => Unbound
    (Just type) => type

infixl 4 |=>
interface WithBindings a b | a where
  (|=>) : Bindings -> a -> b
