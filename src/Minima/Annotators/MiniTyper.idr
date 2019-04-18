module MiniTyper

import Minima.AST
import Minima.Record
import Minima.Annotators.UniqueIndexer
import Minima.Annotators.MTypes
import Minima.Annotators.Bindings
import Control.ST

%access public export

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
  instantiate bindings unbound@(MUnbound index) = lookupMType' bindings index
  instantiate bindings type = type

Typed : List (Type, Type) -> Type -> Type
Typed as index = Expression (Record (('MTyp, MType) :: as)) index

typeOf : Typed as i -> MType
typeOf exp = getField 'MTyp (annotations exp)

returnType : MType -> Either MTypeError MType
returnType (MFunction args ret) = pure ret
returnType _ = Left $ MkTypeError "Not a function type"

lastTypeUnlessNonSuccess : List MType -> Either MTypeError MType
lastTypeUnlessNonSuccess [] = pure MSuccess
lastTypeUnlessNonSuccess [type] = pure type
lastTypeUnlessNonSuccess (MSuccess :: xs) = lastTypeUnlessNonSuccess xs
lastTypeUnlessNonSuccess (type :: _) = Left $ MkTypeError
  $ "Return values of non-terminal group expressions must not be ignored: ignoring a " ++ show type

mutual
  addTypes : (bindings : Var)
          -> Expression (Record as) Index
          -> ST (Either MTypeError) (Typed as Index) [bindings ::: State Bindings]
  addTypes bindings (StringLiteral as text) =
    pure $ StringLiteral ('MTyp := MString :: as) text
  addTypes bindings (NumberLiteral as num) =
    pure $ NumberLiteral ('MTyp := MNumber :: as) num
  addTypes bindings (Variable as index) =
    pure $ Variable ('MTyp := (lookupMType' !(read bindings) index) :: as) index
  addTypes bindings (Definition as index value) = do
    value' <- addTypes bindings value
    newBindings <- lift $ bindType index (typeOf value') !(read bindings)
    write bindings newBindings
    pure $ Definition ('MTyp := MSuccess :: as) index value'
  addTypes bindings (Function as args body) = do
    body' <- addTypes bindings body
    let type = MFunction (MUnbound <$> args) (typeOf body')
    pure $ Function ('MTyp := type :: as) args body'
  addTypes bindings (Call as fun args) = do
    fun' <- addTypes bindings fun
    args' <- addAllTypes bindings args
    let funType = typeOf fun'
    let callType = MFunction (typeOf <$> args') (MUnbound (-1))
    (_, type) <- lift $ unify !(read bindings) funType callType
    retType <- lift $ returnType type
    pure $ Call ('MTyp := retType :: as) fun' args'
  addTypes bindings (Group as exps) = do
    expressions <- addAllTypes bindings exps
    lastType <- lift $ lastTypeUnlessNonSuccess (typeOf <$> expressions)
    pure $ Group ('MTyp := lastType :: as) expressions

  addAllTypes : (bindings : Var)
             -> List (Expression (Record as) Index)
             -> ST (Either MTypeError) (List (Typed as Index)) [bindings ::: State Bindings]
  addAllTypes bindings [] = pure []
  addAllTypes bindings (x :: xs) = do
    first <- addTypes bindings x
    rest <- addAllTypes bindings xs
    pure $ first :: rest

typeExp' : Bindings -> Expression (Record as) Index -> ST (Either MTypeError) (Typed as Index) []
typeExp' bindings exp = do types <- new bindings
                           typed <- addTypes types exp
                           delete types
                           pure typed

typeExp : Expression (Record as) Index -> Either MTypeError (Typed as Index)
typeExp exp = run $ typeExp' [] exp

typeExpWith : Bindings -> Expression (Record as) Index -> Either MTypeError (Typed as Index)
typeExpWith bindings exp = run $ typeExp' bindings exp
