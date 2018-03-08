module MinimaTypes

import Prelude.Either

%access public export

TypeError : Type
TypeError = String

data MinimaType : Type where
  Primitive : String -> MinimaType
  NamedType : String -> MinimaType
  Function : (args : List MinimaType) -> (returns : MinimaType) -> MinimaType

Show MinimaType where
  show (Primitive x) = x
  show (NamedType x) = x
  show (Function args rets) = show args ++ " => " ++ show rets

Eq MinimaType where
  (==) (Primitive x) (Primitive y) = x == y
  (==) (NamedType x) (NamedType y) = x == y
  (==) (Function args1 returns1) (Function args2 returns2) = returns1 == returns2
  (==) _ _ = False

Context : Type
Context = List (String, MinimaType)

resolveType : Context -> String -> Maybe MinimaType
resolveType ctx name = case lookup name ctx of
  Nothing => Nothing
  (Just (NamedType newAlias)) => resolveType ctx newAlias
  (Just concrete) => Just concrete

infixl 4 =>>
interface WithContext a b | a where
  (=>>) : Context -> a -> b

data AssOp = Assignment MinimaType MinimaType
infixl 8 ->?
(->?) : MinimaType -> MinimaType -> AssOp
(->?) = Assignment

mutual
  assignTo : Context -> (src : MinimaType) -> (tgt : MinimaType) -> Maybe TypeError
  assignTo ctx src (NamedType alias) = case resolveType ctx alias of
      Nothing => Just $ "No such type " ++ alias ++ " found"
      (Just tgt) => ctx =>> src ->? tgt
  assignTo ctx (NamedType alias) tgt = case resolveType ctx alias of
      Nothing => Just $ "No such type " ++ alias ++ " found"
      (Just src) => ctx =>> src ->? tgt
  assignTo _ (Primitive src) (Primitive tgt) = case src == tgt of
      False => Just ("Cannot assign " ++ src ++ " to " ++ tgt)
      True => Nothing
  assignTo ctx (Function srcArgs srcReturns) (Function tgtArgs tgtReturns) =
      if (length srcArgs) /= (length tgtArgs)
          then Just "Arity mismatch"
          else let argsErrors = zipWith (assignTo ctx) tgtArgs srcArgs
                   returnsErrors = ctx =>> srcReturns ->? tgtReturns
                in case catMaybes (returnsErrors :: argsErrors) of
                     [] => Nothing
                     (x :: xs) => Just x
  assignTo _ src tgt = Just $ "Cannot assign " ++ show src ++ " to " ++ show tgt

  WithContext AssOp (Maybe TypeError) where
    ctx =>> (Assignment src tgt) = assignTo ctx src tgt

data CallOp = Call MinimaType (List MinimaType)
infixl 8 $?
($?) : MinimaType -> List MinimaType -> CallOp
($?) = Call

mutual
  call : Context -> (fun : MinimaType) -> (args : List MinimaType) -> Either TypeError MinimaType
  call ctx (NamedType x) args = case resolveType ctx x of
    Nothing => Left $ "No such type " ++ x ++ " found"
    (Just x) => ctx =>> x $? args
  call ctx (Function params returns) args = if length params == length args
    then let assignedArgs = zipWith (assignTo ctx) args params
          in case catMaybes assignedArgs of
             [] => Right returns
             (x :: xs) => Left x
    else Left $ "Arity mismatch: " ++ show (length params) ++ " arguments expected, " ++ show (length args) ++ " provided"
  call _ typ _ = Left $ "Type " ++ show typ ++ " is not a function"

  WithContext CallOp (Either TypeError MinimaType) where
    ctx =>> (Call fun args) = call ctx fun args

data PullOp = Pull MinimaType String
infixl 8 :?
(:?) : MinimaType -> String -> PullOp
(:?) = Pull
