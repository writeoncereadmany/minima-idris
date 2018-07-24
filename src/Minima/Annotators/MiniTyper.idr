module MiniTyper

import Minima.AST
import Minima.Record
import Minima.Annotators.UniqueIndexer
import Control.ST

data MType = MString
           | MNumber
           | MSuccess
           | MFunction (List MType) MType
           | MUnbound Index
           | MTypeError String

Show MType where
  show MString = "String"
  show MNumber = "Number"
  show MSuccess = "Success"
  show (MUnbound x) = "Unbound " ++ show x
  show (MTypeError x) = "Type Error: " ++ x
  show (MFunction xs x) = let args = show <$> xs
                              returnValue = show x
                           in show args ++ " -> " ++ returnValue

Typed : List (Type, Type) -> Type -> Type
Typed as index = Expression (Record (('MTyp, MType) :: as)) index

lookupType : Index -> List (Index, MType) -> MType
lookupType i is = case lookup i is of
  Nothing => MTypeError $ "Cannot find type for variable index " ++ show i
  (Just type) => type

typeOf : Typed as i -> MType
typeOf exp = getField 'MTyp (annotations exp)

unify : MType -> MType -> MType
unify a b = ?unify_hole

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
                                         ?call_hole
  addTypes types (Group as exps) = do expressions <- addAllTypes types exps
                                      let type = lastTypeUnlessNonSuccess (typeOf <$> expressions)
                                      pure $ Group ('MTyp := type :: as) expressions

  lastTypeUnlessNonSuccess : List MType -> MType
  lastTypeUnlessNonSuccess [] = MSuccess
  lastTypeUnlessNonSuccess [type] = type
  lastTypeUnlessNonSuccess (MSuccess :: xs) = lastTypeUnlessNonSuccess xs
  lastTypeUnlessNonSuccess (MTypeError msg :: _) = MTypeError msg
  lastTypeUnlessNonSuccess (type :: _) = MTypeError $ "Return values of non-terminal group expressions must not be ignored: ignoring a " ++ show type

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
