module Annotators.Scoper

import Minima.AST
import Minima.Record
import Control.ST

Scope : Type
Scope = List Integer

Scoped : List (Type, Type) -> Type -> Type
Scoped as i = Expression (Record (('Scope, Scope) :: as)) i

-- hmm... how am i going to use this information? is scope a property of an AST node?
-- eg, given a function, the args are defined in the scope of the body:
-- maybe that just means the entire function
-- it would simplify many things if a program were just a group...

addScopes : (current : Var) -> Expression (Record as) i -> ST m (Scoped as i) [current ::: State Scope]
addScopes scope (StringLiteral as text) = pure $ StringLiteral ('Scope := !(read scope) :: as) text
addScopes scope (NumberLiteral as num) = pure $ NumberLiteral ('Scope := !(read scope) :: as) num
addScopes scope (Variable as name) = pure $ Variable ('Scope := !(read scope) :: as) name
addScopes scope (Definition as name value) = do current <- read scope
                                                val <- addScopes scope value
                                                pure $ Definition ('Scope := current :: as) name val
addScopes scope (Function as args body) = ?function
addScopes scope (Call x y xs) = ?call
addScopes scope (Group x xs) = ?group
