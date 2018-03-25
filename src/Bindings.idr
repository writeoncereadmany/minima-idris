module Bindings

import MinimaTypes

%access public export

Binding : Type
Binding = (DeBruijnIndex, MinimaType)

Bindings : Type
Bindings = List Binding

lookupType : DeBruijnIndex -> Bindings -> MinimaType
lookupType i b = case lookup i b of
    Nothing => Unbound i
    (Just type) => type

mergeBindings : Bindings -> Bindings -> Bindings
mergeBindings a b = a ++ b

mergeAll : List Bindings -> Bindings
mergeAll bindings = foldl mergeBindings [] bindings

infixl 4 |=>
interface WithBindings a b | a where
  (|=>) : Bindings -> a -> b
