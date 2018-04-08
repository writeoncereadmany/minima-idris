module NewSemantics

import Minima.AST
import Control.Monad.State

%access public export

record ExpressionSemantics2 a i c r where
  constructor MkExpressionSemantics
  onStringLiteral : a -> String -> State c r
  onNumberLiteral : a -> Integer -> State c r
  onVariable : a -> i -> State c r
  onDefinition : a -> i -> r -> State c r
  onFunction : a -> List i -> Expression a i -> State c r
  onCall : a -> r -> List r -> State c r
  onGroup : a -> List r -> State c r

foldExpression2 : ExpressionSemantics2 a i c r -> Expression a i -> State c r
foldExpression2 apply = foldOver where
  foldOver (StringLiteral a value) = onStringLiteral apply a value
  foldOver (NumberLiteral a value) = onNumberLiteral apply a value
  foldOver (Variable a name) = onVariable apply a name
  foldOver (Definition a name value) = do val <- foldOver value
                                          onDefinition apply a name val
  foldOver (Function a args body) = onFunction apply a args body
  foldOver (Call a fun args) = do f <- foldOver fun
                                  as <- traverse foldOver args
                                  onCall apply a f as
  foldOver (Group a exps) = do es <- traverse foldOver exps
                               onGroup apply a es
