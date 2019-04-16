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

bound : Binding -> List Index
bound (Equivalent indices) = indices
bound (Bound indices type) = indices

Bindings : Type
Bindings = List Binding

boundOverAny : List Index -> Binding -> Bool
boundOverAny indices binding = any (`elem` bound binding) indices

mergeBound : List Index -> Bindings -> Either MTypeError Binding
mergeBound indices bindings = mergeBound' indices Nothing bindings where
  mergeBound' : List Index -> Maybe MType -> Bindings -> Either MTypeError Binding
  mergeBound' indices Nothing [] = pure $ Equivalent indices
  mergeBound' indices (Just type) [] = pure $ Bound indices type
  mergeBound' indices type ((Equivalent indices') :: bindings)
      = mergeBound' (union indices indices') type bindings
  mergeBound' indices Nothing ((Bound indices' type) :: bindings)
      = mergeBound' (union indices indices') (Just type) bindings
  mergeBound' indices (Just type) ((Bound indices' type') :: bindings)
      = if type == type'
        then mergeBound' (union indices indices') (Just type) bindings
        else Left $ MkTypeError $ "Type mismatch: cannot unify " ++ show type ++ " and " ++ show type'

equivalent : Index -> Index -> Bindings -> Either MTypeError Bindings
equivalent index1 index2 bindings = let
     (bound, unbound) = partition (boundOverAny [index1, index2]) bindings
  in [| mergeBound [index1, index2] bound :: pure unbound |]

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
