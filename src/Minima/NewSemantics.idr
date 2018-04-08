module NewSemantics

import Minima.AST

%access public export

record ExpressionSemantics2 a i c r where
  constructor MkExpressionSemantics
  onStringLiteral : c -> a -> String -> (c, r)
  onNumberLiteral : c -> a -> Integer -> (c, r)
  onVariable : c -> a -> i -> (c, r)
  onDefinition : c -> a -> i -> (c, r) -> (c, r)
  onFunction : c -> a -> List i -> Expression a i -> (c, r)
  onCall : c -> a -> (c, r) -> List (c, r) -> (c, r)
  onGroup : c -> a -> List (c, r) -> (c, r)

-- basically scanl, only it doesn't include the initial value
accumulate2 : (a -> b -> (a, c)) -> a -> List b -> List (a, c)
accumulate2 f x [] = []
accumulate2 f x (y :: xs) = let (nc, nv) = f x y
                            in (nc, nv) :: accumulate2 f nc xs

foldExpression2 : ExpressionSemantics2 a i c r -> c -> Expression a i -> (c, r)
foldExpression2 apply = foldOver where
  foldOver context (StringLiteral a value) = onStringLiteral apply context a value
  foldOver context (NumberLiteral a value) = onNumberLiteral apply context a value
  foldOver context (Variable a name) = onVariable apply context a name
  foldOver context (Definition a name value) = onDefinition apply context a name (foldOver context value)
  foldOver context (Function a args body) = onFunction apply context a args body
  foldOver context (Call a fun args) = let (fc, fv) = foldOver context fun
                                           argContexts = accumulate2 foldOver fc args
                                        in onCall apply context a (fc, fv) argContexts
  foldOver context (Group a exps) = onGroup apply context a (accumulate2 foldOver context exps)
