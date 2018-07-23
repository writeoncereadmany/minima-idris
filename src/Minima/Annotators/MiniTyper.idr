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

Typed : List (Type, Type) -> Type -> Type
Typed as index = Expression (Record (('MTyp, MType) :: as)) index

addTypes : (types : Var)
        -> Expression (Record as) Index
        -> ST m (Typed as Index) [types ::: State (List (Index, MType))]
addTypes types (StringLiteral as text) = pure $ StringLiteral ('MTyp := MString :: as) text
addTypes types (NumberLiteral as num) = pure $ NumberLiteral ('MTyp := MNumber :: as) num
addTypes types (Variable as name) = ?addTypes_rhs_3
addTypes types (Definition x y z) = ?addTypes_rhs_4
addTypes types (Function x xs y) = ?addTypes_rhs_5
addTypes types (Call x y xs) = ?addTypes_rhs_6
addTypes types (Group x xs) = ?addTypes_rhs_7

typeExp' : Expression (Record as) Index -> ST m (Typed as Index) []
typeExp' exp = do types <- new []
                  typed <- addTypes types exp
                  delete types
                  pure typed

typeExp : Expression (Record as) Index -> Typed as Index
typeExp exp = runPure $ typeExp' exp
