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

mergeBound : List Index -> Maybe MType -> Bindings -> Either MTypeError Binding
mergeBound indices Nothing [] = pure $ Equivalent indices
mergeBound indices (Just type) [] = pure $ Bound indices type
mergeBound indices type ((Equivalent indices') :: bindings)
    = mergeBound (union indices indices') type bindings
mergeBound indices Nothing ((Bound indices' type) :: bindings)
    = mergeBound (union indices indices') (Just type) bindings
mergeBound indices (Just type) ((Bound indices' type') :: bindings)
    = if type == type'
      then mergeBound (union indices indices') (Just type) bindings
      else Left $ MkTypeError $ "Type mismatch: cannot unify " ++ show type ++ " and " ++ show type'

mergeBindings : List Index -> Maybe MType -> Bindings -> Either MTypeError Bindings
mergeBindings indices type bindings = let
     (overlapping, disjoint) = partition (boundOverAny indices) bindings
  in [| mergeBound indices type overlapping :: pure disjoint |]
  where boundOverAny : List Index -> Binding -> Bool
        boundOverAny indices binding = any (`elem` bound binding) indices

bindType : Index -> MType -> Bindings -> Either MTypeError Bindings
bindType index1 (MUnbound index2) bindings = mergeBindings [index1, index2] Nothing bindings
bindType index type bindings = mergeBindings [index] (Just type) bindings

combineBindings : Bindings -> Bindings -> Either MTypeError Bindings
combineBindings bindings1 bindings2 = foldlM combineBinding bindings1 bindings2 where
  combineBinding : Bindings -> Binding -> Either MTypeError Bindings
  combineBinding bindings (Equivalent indices) = mergeBindings indices Nothing bindings
  combineBinding bindings (Bound indices type) = mergeBindings indices (Just type) bindings

lookupMType : Bindings -> Index -> Either MTypeError MType
lookupMType [] index = Left $ MkTypeError $ "Index " ++ show index ++ " not bound"
lookupMType ((Bound indices type) :: bindings) index = if index `elem` indices
  then pure type
  else lookupMType bindings index
lookupMType ((Equivalent _) :: bindings) index = lookupMType bindings index
