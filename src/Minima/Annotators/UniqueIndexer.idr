module Annotators.UniqueIndexer

import Minima.AST
import Control.ST
import Data.Vect
import Minima.Interpreter.DependentEnvironment

%access public export

NameError:  Type
NameError = String

Index : Type
Index = Integer

lookup : (Show k, Eq k) => k -> Environment d k v -> Either NameError v
lookup k env = case lookupValue k env of
  Nothing => Left $ "Could not resolve name " ++ show k
  (Just x) => Right x

mutual
  indexName : (Show i, Eq i)
            => (env : Var)
            -> (next : Var)
            -> (name : i)
            -> ST (Either NameError) Index [env ::: State (Environment (S d) i Index), next ::: State Index]
  indexName env next name = do index <- read next
                               write next (index + 1)
                               update env (define name index)
                               pure index

  indexNames : (Show i, Eq i)
            => (env : Var)
            -> (next : Var)
            -> (name : List i)
            -> ST (Either NameError) (List Index) [env ::: State (Environment (S d) i Index), next ::: State Index]
  indexNames env next [] = pure []
  indexNames env next (x :: xs) = do first <- indexName env next x
                                     rest <- indexNames env next xs
                                     pure $ first :: rest

  addIndices : (Show i, Eq i)
            => (env : Var)
            -> (next : Var)
            -> (exp : Expression as i)
            -> ST (Either NameError) (Expression as Index) [env ::: State (Environment (S d) i Index), next ::: State Index]
  addIndices env next (StringLiteral as text) = pure $ StringLiteral as text
  addIndices env next (NumberLiteral as num) = pure $ NumberLiteral as num
  addIndices env next (Variable as name) = do env' <- read env
                                              index <- lift $ lookup name env'
                                              pure $ Variable as index
  addIndices env next (Definition as name value) = do index <- indexName env next name
                                                      value' <- addIndices env next value
                                                      pure $ Definition as index value'
  addIndices env next (Function as args body) = do update env enterScope
                                                   args' <- indexNames env next args
                                                   body' <- addIndices env next body
                                                   update env exitScope
                                                   pure $ Function as args' body'
  addIndices env next (Call as fun args) = do fun' <- addIndices env next fun
                                              args' <- overAll env next args
                                              pure $ Call as fun' args'
  addIndices env next (Group as exps) = do update env enterScope
                                           exps' <- overAll env next exps
                                           update env exitScope
                                           pure $ Group as exps'

  overAll : (Show i, Eq i)
         => (env : Var)
         -> (next : Var)
         -> (exps : List (Expression as i))
         -> ST (Either NameError) (List (Expression as Index)) [env ::: State (Environment (S d) i Index), next ::: State Index]
  overAll env next [] = pure []
  overAll env next (x :: xs) = do first <- addIndices env next x
                                  rest <- overAll env next xs
                                  pure (first :: rest)

uniqueIndex' : (Show i, Eq i)
            => Environment d i Index
            -> Expression as i
            -> ST (Either NameError) (Expression as Index) []
uniqueIndex' xs exp = do indices <- new 0
                         env <- new xs
                         update env enterScope
                         indexed <- addIndices env indices exp
                         update env exitScope
                         delete indices
                         delete env
                         pure indexed

uniqueIndex : (Show i, Eq i)
           => Expression as i
           -> Either NameError (Expression as Index)
uniqueIndex exp = run $ uniqueIndex' [] exp


uniqueIndexWith : (Show i, Eq i)
               => Environment d i Index
               -> Expression as i
               -> Either NameError (Expression as Index)
uniqueIndexWith xs exp = run $ uniqueIndex' xs exp
