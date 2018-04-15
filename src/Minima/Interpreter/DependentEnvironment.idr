module DependentEnvironment

import Data.Vect
import Minima.Interpreter.Value

%access public export

Environment : (depth : Nat) -> (key : Type) -> (value : Type) -> Type
Environment depth key value = Vect depth (List (key, value))

lookupValue : (Eq k) => k -> Environment d k v -> Maybe v
lookupValue k env = case catMaybes $ (lookup k) <$> env of
        (_ ** []) => Nothing
        (_ ** (x::xs)) => Just x

enterScope : Environment d k v -> Environment (S d) k v
enterScope env = [] :: env

exitScope : Environment (S d) k v -> Environment d k v
exitScope (x :: xs) = xs

define : k -> v -> Environment (S d) k v -> Environment (S d) k v
define name value (currentScope :: higherScopes) = ((name, value) :: currentScope) :: higherScopes

defineAll : List (k, v) -> Environment (S d) k v -> Environment (S d) k v
defineAll [] env = env
defineAll ((key, value) :: rest) env = define key value (defineAll rest env)
