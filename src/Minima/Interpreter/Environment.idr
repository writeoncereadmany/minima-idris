module Environment

import Minima.Interpreter.Value

%access public export

Environment : Type -> Type -> Type
Environment i n = List (List (n, Value i n))

lookupValue : (Eq n, Show n) => n -> Environment i n -> Either String (Value i n)
lookupValue name env = case catMaybes $ (lookup name) <$> env of
                        [] => Left $ "Variable " ++ show name ++ " is undefined"
                        (x :: xs) => Right x

enterScope : Environment i n -> Environment i n
enterScope existingEnvironment = [] :: existingEnvironment

exitScope : Environment i n -> Environment i n
exitScope [] = ?exitScope_rhs_1
exitScope (currentScope :: higherScopes) = higherScopes

define : n -> Value i n -> Environment i n -> Environment i n
define x y [] = ?define_rhs_1
define name value (currentScope :: higherScopes) = ((name, value) :: currentScope) :: higherScopes

defineAll : List (n, Value i n) -> Environment i n -> Environment i n
defineAll [] env = env
defineAll ((name, value) :: rest) env = define name value (defineAll rest env)
