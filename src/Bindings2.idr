module Bindings2

import MinimaTypes2

%access public export

Binding2 : Type
Binding2 = (DeBruijnIndex, MinimaType2)

Bindings2 : Type
Bindings2 = List Binding2

lookupType : DeBruijnIndex -> Bindings2 -> MinimaType2
lookupType i b = case lookup i b of
    Nothing => Unbound i
    (Just type) => type

mergeBindings : Bindings2 -> Bindings2 -> Bindings2
mergeBindings a b = a

infixl 4 |=>
interface WithBindings2 a b | a where
  (|=>) : Bindings2 -> a -> b
