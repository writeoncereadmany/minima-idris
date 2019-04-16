module Bindings

import Minima.Annotators.UniqueIndexer
import Minima.Annotators.MTypes

%access public export

data Binding = Equivalent (List Index)
             | Bound (List Index) MType

Eq Binding where
  (Equivalent xs) == (Equivalent ys) = xs == ys
  (Bound xs t1) == (Bound ys t2) = xs == ys && t1 == t2
  _ == _ = False

Show Binding where
  show (Equivalent xs) = show xs
  show (Bound xs t) = show xs ++ " -> " ++ show t

Bindings : Type
Bindings = List Binding


equivalent : Index -> Index -> Bindings -> Either MTypeError Bindings
equivalent x y z = ?equivalent_rhs

bindType : Index -> MType -> Bindings -> Either MTypeError Bindings
bindType index type [] = pure $ [ Bound [ index ] type ]
bindType index type (head@(Equivalent indices) :: bindings) = if index `elem` indices
  then pure $ Bound indices type :: bindings
  else [| pure head :: bindType index type bindings |]
bindType index type (head@(Bound indices type') :: bindings) = if index `elem` indices
  then if type == type'
     then pure $ head :: bindings
     else Left $ MkTypeError $ "Type mismatch for index " ++ show index ++ ": both " ++ show type ++ " and " ++ show type' ++ " bound."
  else [| pure head :: bindType index type bindings |]

lookupMType : Bindings -> Index -> Either MTypeError MType
lookupMType [] index = Left $ MkTypeError $ "Index " ++ show index ++ " not bound"
lookupMType ((Bound indices type) :: bindings) index = if index `elem` indices
  then pure type
  else lookupMType bindings index
lookupMType ((Equivalent _) :: bindings) index = lookupMType bindings index
