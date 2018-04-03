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

record ExpressionSemantics a i c where
  constructor MkExpressionSemantics
  onStringLiteral : c -> a -> String -> c
  onNumberLiteral : c -> a -> Integer -> c
  onVariable : c -> a -> i -> c
  onDefinition : c -> a -> i -> c -> c
  onFunction : c -> a -> List i -> Expression a i -> c
  onCall : c -> a -> c -> List c -> c
  onGroup : c -> a -> List c -> c

-- basically scanl, only it doesn't include the initial value
accumulate : (a -> b -> a) -> a -> List b -> List a
accumulate f x [] = []
accumulate f x (y :: xs) = let next = f x y
                            in next :: accumulate f next xs

foldExpression : ExpressionSemantics a i c -> c -> Expression a i -> c
foldExpression apply = foldOver where
  foldOver context (StringLiteral a value) = onStringLiteral apply context a value
  foldOver context (NumberLiteral a value) = onNumberLiteral apply context a value
  foldOver context (Variable a name) = onVariable apply context a name
  foldOver context (Definition a name value) = onDefinition apply context a name (foldOver context value)
  foldOver context (Function a args body) = onFunction apply context a args body
  foldOver context (Call a fun args) = let fContext = foldOver context fun
                                           argContexts = accumulate foldOver fContext args
                                        in onCall apply context a fContext argContexts
  foldOver context (Group a exps) = onGroup apply context a (accumulate foldOver context exps)


joinWith : String -> List String -> String
joinWith sep [] = ""
joinWith sep (x :: xs) = joinWith' xs x where
  joinWith' : List String -> String -> String
  joinWith' [] a = a
  joinWith' (x :: xs) a = joinWith' xs (a ++ sep ++ x)


Show i => Show (Expression a i) where
  show (StringLiteral a string) = "\"" ++ string ++ "\""
  show (NumberLiteral a num) = show num
  show (Variable a var) = show var
  show (Definition a name exp) = show name ++ " is " ++ show exp
  show (Function a args body) = show args ++ " => " ++ show body
  show (Call a fun args) = show fun ++ show args
  show (Group a xs) = "(" ++ joinWith ", " (show <$> xs) ++ ")"

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
