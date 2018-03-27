module AST

%access public export

-- not touching on objects quite yet, start with data and functions
data Expression a = StringLiteral a String
                  | NumberLiteral a Integer
                  | Variable a String
                  | Definition a String (Expression a)
                  | Function a (List String) (Expression a)
                  | Call a (Expression a) (List (Expression a))
                  | Group a (List (Expression a))

joinWith : String -> List String -> String
joinWith sep [] = ""
joinWith sep (x :: xs) = joinWith' xs x where
  joinWith' : List String -> String -> String
  joinWith' [] a = a
  joinWith' (x :: xs) a = joinWith' xs (a ++ sep ++ x)


Show (Expression a) where
  show (StringLiteral a string) = "\"" ++ string ++ "\""
  show (NumberLiteral a num) = show num
  show (Variable a var) = var
  show (Definition a name exp) = name ++ " is " ++ show exp
  show (Function a args body) = show args ++ " => " ++ show body
  show (Call a fun args) = show fun ++ show args
  show (Group a xs) = "(" ++ joinWith ", " (show <$> xs) ++ ")"

mutual
  Eq a => Eq (Expression a) where
    (==) (StringLiteral a x) (StringLiteral b y) = x == y
    (==) (NumberLiteral a x) (NumberLiteral b y) = x == y
    (==) (Variable a x) (Variable b y) = x == y
    (==) (Definition a n1 e1) (Definition b n2 e2) = n1 == n2 && e1 == e2
    (==) (Function a args1 body1) (Function b args2 body2) = args1 == args2 && body1 == body2
    (==) (Call a fun1 args1) (Call b fun2 args2) = fun1 == fun2 && allEq args1 args2
    (==) (Group a xs) (Group b ys) = allEq xs ys
    (==) _ _ = False

  allEq : Eq a => List (Expression a) -> List (Expression a) -> Bool
  allEq [] [] = True
  allEq [] (x :: xs) = False
  allEq (x :: xs) [] = False
  allEq (x :: xs) (y :: ys) = x == y && allEq xs ys
