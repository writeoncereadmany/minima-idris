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
  eq : (a -> a -> Bool) -> Expression a -> Expression a -> Bool
  eq f (StringLiteral a x) (StringLiteral b y) = f a b && x == y
  eq f (NumberLiteral a x) (NumberLiteral b y) = f a b && x == y
  eq f (Variable a x) (Variable b y) = f a b && x == y
  eq f (Definition a n1 e1) (Definition b n2 e2) = f a b && n1 == n2 && eq f e1 e2
  eq f (Function a args1 body1) (Function b args2 body2) = f a b && args1 == args2 && eq f body1 body2
  eq f (Call a fun1 args1) (Call b fun2 args2) = f a b && eq f fun1 fun2 && allEq f args1 args2
  eq f (Group a xs) (Group b ys) = f a b && allEq f xs ys
  eq f _ _ = False

  allEq : (a -> a -> Bool) -> List (Expression a) -> List (Expression a) -> Bool
  allEq f [] [] = True
  allEq f [] (x :: xs) = False
  allEq f (x :: xs) [] = False
  allEq f (x :: xs) (y :: ys) = eq f x y && allEq f xs ys

Eq a => Eq (Expression a) where
  (==) = eq (==)
