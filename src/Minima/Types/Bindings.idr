module Bindings

import Minima.Types.MinimaTypes

%access public export

Binding : Type
Binding = (DeBruijnIndex, MinimaType)

Bindings : Type
Bindings = List Binding

lookupType : DeBruijnIndex -> Bindings -> MinimaType
lookupType i b = case lookup i b of
    Nothing => Unbound i
    (Just type) => type

resolveType : MinimaType -> Bindings -> MinimaType
resolveType (Named introduction) bindings = lookupType introduction bindings
resolveType concrete _ = concrete

infixl 4 |=>
interface WithBindings a b | a where
  (|=>) : Bindings -> a -> b
