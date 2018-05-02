module Annotators.Scoper

import Data.Vect
import Minima.AST
import Minima.Record
import Control.ST

%access public export

Scope : Type
Scope = List Integer

Scoped : List (Type, Type) -> Type -> Type
Scoped as i = Expression (Record (('Scope, Scope) :: as)) i

enterScope : (current : Var) -> (next : Var) -> ST m () [
  current ::: State (Vect n Integer) :-> State (Vect (S n) Integer),
  next ::: State Integer]
enterScope current next = do scope <- read current
                             nxt <- read next
                             write next 0
                             write current (nxt :: scope)
                             pure ()

exitScope : (current : Var) -> (next : Var) -> ST m () [
  current ::: State (Vect (S n) Integer) :-> State (Vect n Integer),
  next ::: State Integer]
exitScope current next = do (head :: rest) <- read current
                            write next (head + 1)
                            write current rest
                            pure ()

withScope : Vect n Integer -> Record as -> Record (('Scope, Scope) :: as)
withScope scope as = ('Scope := toList scope :: as)

mutual
  addScopes : (current : Var)
           -> (next : Var)
           -> Expression (Record as) i
           -> ST m (Scoped as i) [current ::: State (Vect n Integer), next ::: State Integer]
  addScopes scope next (StringLiteral as text) = pure $ StringLiteral (withScope !(read scope) as) text
  addScopes scope next (NumberLiteral as num) = pure $ NumberLiteral (withScope !(read scope) as) num
  addScopes scope next (Variable as name) = pure $ Variable (withScope !(read scope) as) name
  addScopes scope next (Definition as name value) = do val <- addScopes scope next value
                                                       pure $ Definition (withScope !(read scope) as) name val
  addScopes scope next (Function as args body) = do enterScope scope next
                                                    scope' <- read scope
                                                    body' <- addScopes scope next body
                                                    exitScope scope next
                                                    pure $ Function (withScope scope' as) args body'
  addScopes scope next (Call as fun args) = do fun' <- addScopes scope next fun
                                               args' <- addAllScopes scope next args
                                               pure $ Call (withScope !(read scope) as) fun' args'
  addScopes scope next (Group as exps) = do enterScope scope next
                                            scope' <- read scope
                                            exps' <- addAllScopes scope next exps
                                            exitScope scope next
                                            pure $ Group (withScope scope' as) exps'
  addAllScopes : (current : Var)
              -> (next : Var)
              -> List (Expression (Record as) i)
              -> ST m (List (Scoped as i)) [current ::: State (Vect n Integer), next ::: State Integer]
  addAllScopes scope next [] = pure []
  addAllScopes scope next (x :: xs) = do first <- addScopes scope next x
                                         rest <- addAllScopes scope next xs
                                         pure $ first :: rest

doScope : Expression (Record as) i -> ST m (Scoped as i) []
doScope exp = do next <- new 0
                 current <- new []
                 result <- addScopes current next exp
                 delete current
                 delete next
                 pure result

scope : Expression (Record as) i -> Scoped as i
scope exp = runPure (runScope exp)
