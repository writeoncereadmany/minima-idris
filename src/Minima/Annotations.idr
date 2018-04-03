module Annotations

import Minima.AST
import Minima.Whatever

%access public export

mapAnnotationStringLiteral : (a -> b) -> Expression b i -> a -> String -> Expression b i
mapAnnotationStringLiteral f _ a value = StringLiteral (f a) value

mapAnnotationNumberLiteral : (a -> b) -> Expression b i -> a -> Integer -> Expression b i
mapAnnotationNumberLiteral f _ a value = NumberLiteral (f a) value

mapAnnotationVariable : (a -> b) -> Expression b i -> a -> i -> Expression b i
mapAnnotationVariable f _ a name = Variable (f a) name

mapAnnotationDefinition : (a -> b) -> Expression b i -> a -> i -> Expression b i -> Expression b i
mapAnnotationDefinition f _ a name value = Definition (f a) name value

mapAnnotationCall : (a -> b) -> Expression b i -> a -> Expression b i -> List (Expression b i) -> Expression b i
mapAnnotationCall f _ a fun args = Call (f a) fun args

mapAnnotationGroup : (a -> b) -> Expression b i -> a -> List (Expression b i) -> Expression b i
mapAnnotationGroup f _ a seq = Group (f a) seq

mutual
  mapAnnotationFunction : (a -> b) -> Expression b i -> a -> List i -> Expression a i -> Expression b i
  mapAnnotationFunction f eg a args body = Function (f a) args (foldExpression (annotate f) eg body)

  annotate : (a -> b) -> ExpressionSemantics a i (Expression b i)
  annotate f = MkExpressionSemantics
                (mapAnnotationStringLiteral f)
                (mapAnnotationNumberLiteral f)
                (mapAnnotationVariable f)
                (mapAnnotationDefinition f)
                (mapAnnotationFunction f)
                (mapAnnotationCall f)
                (mapAnnotationGroup f)

stripAnnotations : Whatever i => Expression a i -> Expression () i
stripAnnotations ex = foldExpression (annotate (const ())) (Variable () whatever) ex
