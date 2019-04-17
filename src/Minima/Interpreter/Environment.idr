module Environment

import Minima.Interpreter.Value

%access public export

Environment : (key : Type) -> (value : Type) -> Type
Environment key value = List (List (key, value))

lookupValue : Eq k => k -> Environment k v -> Maybe v
lookupValue name env = case catMaybes $ (lookup name) <$> env of
                        [] => Nothing
                        (x :: xs) => Just x

enterScope : Environment k v -> Environment k v
enterScope existingEnvironment = [] :: existingEnvironment

exitScope : Environment k v -> Environment k v
exitScope [] = []
exitScope (currentScope :: higherScopes) = higherScopes

define : k -> v -> Environment k v -> Environment k v
define x y [] = []
define name value (currentScope :: higherScopes) = ((name, value) :: currentScope) :: higherScopes

defineAll : List (k, v) -> Environment k v -> Environment k v
defineAll [] env = env
defineAll ((key, value) :: rest) env = define key value (defineAll rest env)
