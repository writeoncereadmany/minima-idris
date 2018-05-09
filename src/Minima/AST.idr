module AST

%access public export

-- not touching on objects quite yet, start with data and functions
data Expression a i = StringLiteral a String
                    | NumberLiteral a Integer
                    | Variable a i
                    | Definition a i (Expression a i)
                    | Function a (List i) (Expression a i)
                    | Call a (Expression a i) (List (Expression a i))
                    | Group a (List (Expression a i))

joinWith : String -> List String -> String
joinWith sep strings = pack $ intercalate (unpack sep) (unpack <$> strings)

Show i => Show (Expression a i) where
  show (StringLiteral a string) = "\"" ++ string ++ "\""
  show (NumberLiteral a num) = show num
  show (Variable a var) = show var
  show (Definition a name exp) = show name ++ " is " ++ show exp
  show (Function a args body) = show args ++ " => " ++ show body
  show (Call a fun args) = show fun ++ show args
  show (Group a xs) = "(" ++ joinWith ", " (show <$> xs) ++ ")"

annotations : Expression a i -> a
annotations (StringLiteral x _) = x
annotations (NumberLiteral x _) = x
annotations (Variable x _) = x
annotations (Definition x _ _) = x
annotations (Function x _ _) = x
annotations (Call x _ _) = x
annotations (Group x _) = x


mutual
  eq : (Eq i) => (a -> a -> Bool) -> Expression a i -> Expression a i -> Bool
  eq f (StringLiteral a x) (StringLiteral b y) = f a b && x == y
  eq f (NumberLiteral a x) (NumberLiteral b y) = f a b && x == y
  eq f (Variable a x) (Variable b y) = f a b && x == y
  eq f (Definition a n1 e1) (Definition b n2 e2) = f a b && n1 == n2 && eq f e1 e2
  eq f (Function a args1 body1) (Function b args2 body2) = f a b && args1 == args2 && eq f body1 body2
  eq f (Call a fun1 args1) (Call b fun2 args2) = f a b && eq f fun1 fun2 && allEq f args1 args2
  eq f (Group a xs) (Group b ys) = f a b && allEq f xs ys
  eq f _ _ = False

  allEq : (Eq i) => (a -> a -> Bool) -> List (Expression a i) -> List (Expression a i) -> Bool
  allEq f [] [] = True
  allEq f [] (x :: xs) = False
  allEq f (x :: xs) [] = False
  allEq f (x :: xs) (y :: ys) = eq f x y && allEq f xs ys

(Eq a, Eq i) => Eq (Expression a i) where
  (==) = eq (==)
