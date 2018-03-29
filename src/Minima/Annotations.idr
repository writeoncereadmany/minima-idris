module Annotations

import Minima.AST

%access public export

mapAnnotationStringLiteral : (a -> b) -> Expression b -> a -> String -> Expression b
mapAnnotationStringLiteral f _ a value = StringLiteral (f a) value

mapAnnotationNumberLiteral : (a -> b) -> Expression b -> a -> Integer -> Expression b
mapAnnotationNumberLiteral f _ a value = NumberLiteral (f a) value

mapAnnotationVariable : (a -> b) -> Expression b -> a -> String -> Expression b
mapAnnotationVariable f _ a name = Variable (f a) name

mapAnnotationDefinition : (a -> b) -> Expression b -> a -> String -> Expression b -> Expression b
mapAnnotationDefinition f _ a name value = Definition (f a) name value

mapAnnotationCall : (a -> b) -> Expression b -> a -> Expression b -> List (Expression b) -> Expression b
mapAnnotationCall f _ a fun args = Call (f a) fun args

mapAnnotationGroup : (a -> b) -> Expression b -> a -> List (Expression b) -> Expression b
mapAnnotationGroup f _ a seq = Group (f a) seq

mutual
  mapAnnotationFunction : (a -> b) -> Expression b -> a -> List String -> Expression a -> Expression b
  mapAnnotationFunction f eg a args body = Function (f a) args (foldExpression (annotate f) eg body)

  annotate : (a -> b) -> ExpressionSemantics a (Expression b)
  annotate f = MkExpressionSemantics
                (mapAnnotationStringLiteral f)
                (mapAnnotationNumberLiteral f)
                (mapAnnotationVariable f)
                (mapAnnotationDefinition f)
                (mapAnnotationFunction f)
                (mapAnnotationCall f)
                (mapAnnotationGroup f)

stripAnnotations : Expression a -> Expression ()
stripAnnotations ex = foldExpression (annotate (const ())) (Variable () "") ex
