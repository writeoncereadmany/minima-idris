module MinimaTypes

import Prelude.Either

%access public export

TypeError : Type
TypeError = String

data MinimaType : Type where
  Primitive : String -> MinimaType
  Alias : String -> MinimaType
  Function : (args : List MinimaType) -> (returns : MinimaType) -> MinimaType
  Parameter : String -> MinimaType

Show MinimaType where
  show (Primitive x) = x
  show (Alias x) = x
  show (Function args rets) = show args ++ " => " ++ show rets
  show (Parameter x) = x

Eq MinimaType where
  (==) (Primitive x) (Primitive y) = x == y
  (==) (Alias x) (Alias y) = x == y
  (==) (Function args1 returns1) (Function args2 returns2) = returns1 == returns2
  (==) (Parameter x) (Parameter y) = x == y
  (==) _ _ = False

record Context where
  constructor MkContext
  aliases, parametrics : List (String, MinimaType)

emptyContext : Context
emptyContext = MkContext [] []

Eq Context where
  (==) ctx1 ctx2 = aliases ctx1 == aliases ctx2 && parametrics ctx1 == parametrics ctx2

Show Context where
  show ctx = "Aliases: " ++ show (aliases ctx) ++ ", Parameters: " ++ show (parametrics ctx)

resolve : (Context -> List (String, MinimaType)) -> Context -> String -> Maybe MinimaType
resolve f ctx name = case lookup name (f ctx) of
  Nothing => Nothing
  (Just (Alias newAlias)) => resolve f ctx newAlias
  (Just concrete) => Just concrete

resolveAlias : Context -> String -> Maybe MinimaType
resolveAlias = resolve aliases

resolveParameter : Context -> String -> Maybe MinimaType
resolveParameter = resolve parametrics

infixl 4 =>>
interface WithContext a b | a where
  (=>>) : Context -> a -> b

data AssOp = Assignment MinimaType MinimaType
infixl 8 ->?
(->?) : MinimaType -> MinimaType -> AssOp
(->?) = Assignment

mutual
  assignTo : Context -> (src : MinimaType) -> (tgt : MinimaType) -> Either TypeError Context
  assignTo ctx src (Parameter param) = Right $ record { parametrics $= ((param, src) ::) } ctx
  assignTo ctx src (Alias alias) = case resolveAlias ctx alias of
      Nothing => Left $ "No such type " ++ alias ++ " found"
      (Just tgt) => ctx =>> src ->? tgt
  assignTo ctx (Alias alias) tgt = case resolveAlias ctx alias of
      Nothing => Left $ "No such type " ++ alias ++ " found"
      (Just src) => ctx =>> src ->? tgt
  assignTo ctx (Primitive src) (Primitive tgt) = case src == tgt of
      False => Left ("Cannot assign " ++ src ++ " to " ++ tgt)
      True => Right ctx
  assignTo ctx (Function srcArgs srcReturns) (Function tgtArgs tgtReturns) =
      if (length srcArgs) /= (length tgtArgs)
          then Left "Arity mismatch"
          else let argsErrors = zipWith (assignTo ctx) tgtArgs srcArgs
                   returnsErrors = ctx =>> srcReturns ->? tgtReturns
                in case lefts (returnsErrors :: argsErrors) of
                     [] => Right ctx
                     (x :: xs) => Left x
  assignTo _ src tgt = Left $ "Cannot assign " ++ show src ++ " to " ++ show tgt

  WithContext AssOp (Either TypeError Context) where
    ctx =>> (Assignment src tgt) = assignTo ctx src tgt

data CallOp = Call MinimaType (List MinimaType)
infixl 8 $?
($?) : MinimaType -> List MinimaType -> CallOp
($?) = Call

allEqual : (Eq a) => List a -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x :: rest@(y :: ys)) = x == y && allEqual rest

mutual
  resolve_returntype : (ctx : Context) -> (returns : MinimaType) -> (assignedArgs : List (Either TypeError Context)) -> Either String MinimaType
  resolve_returntype ctx (Parameter p) assignedArgs =
    let possibleTypes = (flip resolveParameter p) <$> (ctx :: rights assignedArgs)
     in case catMaybes possibleTypes of
       [] => Right $ Parameter p
       bindings@(x :: xs) => if allEqual bindings
         then Right x
         else Left $ "Incompatible bindings: " ++ p ++ " cannot be all of " ++ show bindings
  resolve_returntype ctx returns assignedArgs = Right returns

  call : Context -> (fun : MinimaType) -> (args : List MinimaType) -> Either TypeError MinimaType
  call ctx (Alias x) args = case resolveAlias ctx x of
    Nothing => Left $ "No such type " ++ x ++ " found"
    (Just x) => ctx =>> x $? args
  call ctx (Function params returns) args = if length params == length args
    then let assignedArgs = zipWith (assignTo ctx) args params
          in case lefts assignedArgs of
             [] => resolve_returntype ctx returns assignedArgs
             (x :: xs) => Left x
    else Left $ "Arity mismatch: " ++ show (length params) ++ " arguments expected, " ++ show (length args) ++ " provided"
  call _ typ _ = Left $ "Type " ++ show typ ++ " is not a function"

  WithContext CallOp (Either TypeError MinimaType) where
    ctx =>> (Call fun args) = call ctx fun args

data PullOp = Pull MinimaType String
infixl 8 :?
(:?) : MinimaType -> String -> PullOp
(:?) = Pull
