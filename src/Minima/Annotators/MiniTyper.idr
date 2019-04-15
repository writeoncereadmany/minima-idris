module MiniTyper

import Minima.AST
import Minima.Record
import Minima.Annotators.UniqueIndexer
import Control.ST

%access public export

data MType = MString
           | MNumber
           | MSuccess
           | MFunction (List MType) MType
           | MUnbound Index

data MTypeError = MkTypeError String

mutual
  Eq MType where
    (==) MString MString = True
    (==) MNumber MNumber = True
    (==) MSuccess MSuccess = True
    (==) (MUnbound x) (MUnbound y) = x == y
    (==) (MFunction args1 ret1) (MFunction args2 ret2) = allEq args1 args2 && ret1 == ret2
    (==) _ _ = False

  allEq : List MType -> List MType -> Bool
  allEq [] [] = True
  allEq [] (_ :: _) = False
  allEq (_ :: _) [] = False
  allEq (x :: xs) (y :: ys) = x == y && allEq xs ys

Eq MTypeError where
  (==) (MkTypeError a) (MkTypeError b) = a == b

Show MType where
  show MString = "String"
  show MNumber = "Number"
  show MSuccess = "Success"
  show (MUnbound x) = "Unbound " ++ show x
  show (MFunction xs x) = show xs ++ " -> " ++ show x

Show MTypeError where
  show (MkTypeError x) = "Type Error: " ++ x

-- Typed : List (Type, Type) -> Type -> Type
-- Typed as index = Expression (Record (('MTyp, MType) :: as)) index

Binding : Type
Binding = (Index, MType)

Bindings : Type
Bindings = List Binding

lookupType : Index -> Bindings -> Either MTypeError MType
lookupType i is = case lookup i is of
  Nothing => Left $ MkTypeError $ "Cannot find type for variable index " ++ show i
  (Just type) => pure type

combine : Bindings -> Binding -> Either MTypeError Bindings
combine bindings binding@(index, type) = case lookupType index bindings of
  (Left l) => pure (binding :: bindings)
  (Right boundType) => if type == boundType
    then pure bindings
    else Left $ MkTypeError $ "Type mismatch found"

combineBindings: Bindings -> Bindings -> Either MTypeError Bindings
combineBindings xs [] = pure $ xs
combineBindings xs (y :: ys) = do
  xs' <- combine xs y
  combineBindings xs' ys

-- typeOf : Typed as i -> MType
-- typeOf exp = getField 'MTyp (annotations exp)


zipWithM : (Monad m) => (a -> b -> m c) -> List a -> List b -> m (List c)
zipWithM f [] _ = pure []
zipWithM f _ [] = pure []
zipWithM f (x :: xs) (y :: ys) = do
  first <- f x y
  rest <- zipWithM f xs ys
  pure $ first :: rest

liftBindings : List (Bindings, MType) -> Either MTypeError (Bindings, List MType)
liftBindings [] = pure $ ([], [])
liftBindings [(b1, t1), (b2, t2)] = do
  bindings <- combineBindings b1 b2
  pure (bindings, [t1, t2])
liftBindings ((b1, t1) :: xs) = do
  (b2, t2) <- liftBindings xs
  bindings <- combineBindings b1 b2
  pure (bindings, t1 :: t2)

mutual
  unify : Bindings -> MType -> MType -> Either MTypeError (Bindings, MType)
  unify _ MString MString = pure ([], MString)
  unify _ MNumber MNumber = pure ([], MNumber)
  unify _ MSuccess MSuccess = pure ([], MSuccess)
  unify _ (MUnbound i) b = pure ([(i, b)], b)
  unify _ a (MUnbound i) = pure ([(i, a)], a)
  unify bs a@(MFunction args1 ret1) b@(MFunction args2 ret2) = unifyFunctions bs args1 args2 ret1 ret2
  unify _ a b = Left $ MkTypeError $ "Cannot unify " ++ show a ++ " and " ++ show b

  unifyFunctions : Bindings -> List MType -> List MType -> MType -> MType -> Either MTypeError (Bindings, MType)
  unifyFunctions b args1 args2 ret1 ret2 = do
      when (length args1 /= length args2) (Left $ MkTypeError "Arity mismatch")
      argsAndBindings <- zipWithM (unify b) args1 args2
      (b1, args) <- liftBindings argsAndBindings
      (b2, ret) <- unify b ret1 ret2
      bindings <- combineBindings b1 b2
      let boundArgs = instantiate bindings <$> args
      let boundRet = instantiate bindings ret
      pure $ (b, MFunction boundArgs boundRet)

  instantiate : Bindings -> MType -> MType
  instantiate [] unbound@(MUnbound y) = unbound
  instantiate ((index, type) :: bindings) unbound@(MUnbound y) = if index == y
    then type
    else instantiate bindings unbound
  instantiate x type = type

{-
mutual
  addTypes : (types : Var)
          -> Expression (Record as) Index
          -> ST m (Typed as Index) [types ::: State (List (Index, MType))]
  addTypes types (StringLiteral as text) = pure $ StringLiteral ('MTyp := MString :: as) text
  addTypes types (NumberLiteral as num) = pure $ NumberLiteral ('MTyp := MNumber :: as) num
  addTypes types (Variable as name) = do typeList <- read types
                                         let type = lookupType name typeList
                                         pure $ Variable ('MTyp := type :: as) name
  addTypes types (Definition as name value) = do value' <- addTypes types value
                                                 let type = typeOf value'
                                                 update types ((name, type) ::)
                                                 pure $ Definition ('MTyp := MSuccess :: as) name value'
  addTypes types (Function as args body) = do let paramTypes = (\x => (x, MUnbound x)) <$> args
                                              let params = snd <$> paramTypes
                                              update types (paramTypes ++)
                                              body' <- addTypes types body
                                              let type = MFunction params (typeOf body')
                                              pure $ Function ('MTyp := type :: as) args body'
  -- call is the tricky one: this is where we need to unify types, and the only stage we can get type errors
  addTypes types (Call as fun args) = do fun' <- addTypes types fun
                                         args' <- addAllTypes types args
                                         let funtype = typeOf fun'
                                         let argtypes = typeOf <$> args'
                                         let unified = unify funtype (MFunction argtypes (MUnbound (-1)))
                                         pure $ Call ('MTyp := unified :: as) fun' args'
  addTypes types (Group as exps) = do expressions <- addAllTypes types exps
                                      let type = lastTypeUnlessNonSuccess (typeOf <$> expressions)
                                      pure $ Group ('MTyp := type :: as) expressions

  lastTypeUnlessNonSuccess : List MType -> MType
  lastTypeUnlessNonSuccess [] = MSuccess
  lastTypeUnlessNonSuccess [type] = type
  lastTypeUnlessNonSuccess (MSuccess :: xs) = lastTypeUnlessNonSuccess xs
  lastTypeUnlessNonSuccess (MTypeError msg :: _) = MTypeError msg
  lastTypeUnlessNonSuccess (type :: _) = MTypeError
    $ "Return values of non-terminal group expressions must not be ignored: ignoring a " ++ show type

  addAllTypes : (types : Var)
             -> List (Expression (Record as) Index)
             -> ST m (List (Typed as Index)) [types ::: State (List (Index, MType))]
  addAllTypes types [] = pure []
  addAllTypes types (x :: xs) = do
    first <- addTypes types x
    rest <- addAllTypes types xs
    pure $ first :: rest

typeExp' : Expression (Record as) Index -> ST m (Typed as Index) []
typeExp' exp = do types <- new []
                  typed <- addTypes types exp
                  delete types
                  pure typed

typeExp : Expression (Record as) Index -> Typed as Index
typeExp exp = runPure $ typeExp' exp
-}
