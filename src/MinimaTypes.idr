module MinimaTypes

import Prelude.Either

%access public export

TypeError : Type
TypeError = String

data MinimaType : Type where
  Primitive : String -> MinimaType
  NamedType : String -> MinimaType
  Function : (args : List MinimaType) -> (returns : MinimaType) -> MinimaType

data AssOp = Assignment MinimaType MinimaType

infixl 8 ->?
(->?) : MinimaType -> MinimaType -> AssOp
(->?) = Assignment

Context : Type
Context = List (String, MinimaType)

lookupType : Context -> String -> Maybe MinimaType
lookupType [] x = Nothing
lookupType ((name, type) :: rest) typename = if name == typename
  then Just type
  else lookupType rest typename

resolveType : Context -> String -> Maybe MinimaType
resolveType ctx name = case lookupType ctx name of
  Nothing => Nothing
  (Just (NamedType newAlias)) => resolveType ctx newAlias
  (Just concrete) => Just concrete

Show MinimaType where
  show (Primitive x) = x
  show (NamedType x) = x
  show (Function args rets) = show args ++ " => " ++ show rets

assignTo : Context -> (src : MinimaType) -> (tgt : MinimaType) -> Maybe TypeError
assignTo ctx src (NamedType alias) = case resolveType ctx alias of
    Nothing => Just $ "No such type " ++ alias ++ " found"
    (Just tgt) => assignTo ctx src tgt
assignTo ctx (NamedType alias) tgt = case resolveType ctx alias of
    Nothing => Just $ "No such type " ++ alias ++ " found"
    (Just src) => assignTo ctx src tgt
assignTo _ (Primitive src) (Primitive tgt) = case src == tgt of
    False => Just ("Cannot assign " ++ src ++ " to " ++ tgt)
    True => Nothing
assignTo ctx (Function srcArgs srcReturns) (Function tgtArgs tgtReturns) =
    if (length srcArgs) /= (length tgtArgs)
        then Just "Arity mismatch"
        else let argsErrors = zipWith (assignTo ctx) tgtArgs srcArgs 
                 returnsErrors = assignTo ctx srcReturns tgtReturns
              in case catMaybes (returnsErrors :: argsErrors) of
                   [] => Nothing
                   (x :: xs) => Just x
assignTo _ src tgt = Just $ "Cannot assign " ++ show src ++ " to " ++ show tgt

infixl 4 =>>
(=>>) : Context -> AssOp -> Maybe TypeError
ctx =>> (Assignment src tgt) = assignTo ctx src tgt
