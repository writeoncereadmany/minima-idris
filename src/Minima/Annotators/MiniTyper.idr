module MiniTyper

import Minima.AST
import Minima.Record
import Minima.Annotators.UniqueIndexer
import Minima.Annotators.MTypes
import Minima.Annotators.Bindings
import Control.ST

%access public export

-- Typed : List (Type, Type) -> Type -> Type
-- Typed as index = Expression (Record (('MTyp, MType) :: as)) index

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
  unify bindings (MUnbound i) b = do bindings' <- bindType i b bindings
                                     pure (bindings', b)
  unify bindings a (MUnbound i) = do bindings' <- bindType i a bindings
                                     pure (bindings', a)
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
  instantiate bindings unbound@(MUnbound index) = case lookupMType bindings index of
    (Left error) => unbound
    (Right type) => type
  instantiate bindings type = type

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
