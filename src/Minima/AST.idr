module AST

%access public export

-- not touching on objects quite yet, start with data and functions
data Expression = StringLiteral String
                | NumberLiteral Integer
                | Variable String
                | Definition String Expression
                | Function (List String) Expression
                | Call Expression (List Expression)
                | Group (List Expression)

joinWith : String -> List String -> String
joinWith sep [] = ""
joinWith sep (x :: xs) = joinWith' xs x where
  joinWith' : List String -> String -> String
  joinWith' [] a = a
  joinWith' (x :: xs) a = joinWith' xs (a ++ sep ++ x)


Show Expression where
  show (StringLiteral string) = "\"" ++ string ++ "\""
  show (NumberLiteral num) = show num
  show (Variable var) = var
  show (Definition name exp) = name ++ " is " ++ show exp
  show (Function args body) = show args ++ " => " ++ show body
  show (Call fun args) = show fun ++ show args
  show (Group xs) = "(" ++ joinWith ", " (show <$> xs) ++ ")"

mutual
  Eq Expression where
    (==) (StringLiteral x) (StringLiteral y) = x == y
    (==) (NumberLiteral x) (NumberLiteral y) = x == y
    (==) (Variable x) (Variable y) = x == y
    (==) (Definition n1 e1) (Definition n2 e2) = n1 == n2 && e1 == e2
    (==) (Function args1 body1) (Function args2 body2) = args1 == args2 && body1 == body2
    (==) (Call fun1 args1) (Call fun2 args2) = fun1 == fun2 && allEq args1 args2
    (==) (Group xs) (Group ys) = allEq xs ys
    (==) _ _ = False

  allEq : List Expression -> List Expression -> Bool
  allEq [] [] = True
  allEq [] (x :: xs) = False
  allEq (x :: xs) [] = False
  allEq (x :: xs) (y :: ys) = x == y && allEq xs ys
