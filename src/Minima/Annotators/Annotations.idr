module Annotations

import Minima.AST
import Minima.NewSemantics
import Minima.Whatever

%access public export

mapAnnotationStringLiteral : (a -> b) -> () -> a -> String -> ((), Expression b i)
mapAnnotationStringLiteral f () a value = ((), StringLiteral (f a) value)

mapAnnotationNumberLiteral : (a -> b) -> () -> a -> Integer -> ((), Expression b i)
mapAnnotationNumberLiteral f () a value = ((), NumberLiteral (f a) value)

mapAnnotationVariable : (a -> b) -> () -> a -> i -> ((), Expression b i)
mapAnnotationVariable f () a name = ((), Variable (f a) name)

mapAnnotationDefinition : (a -> b) -> () -> a -> i -> ((), Expression b i) -> ((), Expression b i)
mapAnnotationDefinition f () a name ((), value) = ((), Definition (f a) name value)

mapAnnotationCall : (a -> b) -> () -> a -> ((), Expression b i) -> List ((), Expression b i) -> ((), Expression b i)
mapAnnotationCall f () a ((), fun) args = ((), Call (f a) fun (snd <$> args))

mapAnnotationGroup : (a -> b) -> () -> a -> List ((), Expression b i) -> ((), Expression b i)
mapAnnotationGroup f () a seq = ((), Group (f a) (snd <$> seq))

mutual
  mapAnnotationFunction : (a -> b) -> () -> a -> List i -> Expression a i -> ((), Expression b i)
  mapAnnotationFunction f () a args body = ((), Function (f a) args (snd $ foldExpression2 (annotate f) () body))

  annotate : (a -> b) -> ExpressionSemantics2 a i () (Expression b i)
  annotate f = MkExpressionSemantics
                (mapAnnotationStringLiteral f)
                (mapAnnotationNumberLiteral f)
                (mapAnnotationVariable f)
                (mapAnnotationDefinition f)
                (mapAnnotationFunction f)
                (mapAnnotationCall f)
                (mapAnnotationGroup f)

stripAnnotations : Whatever i => Expression a i -> Expression () i
stripAnnotations ex = snd $ foldExpression2 (annotate (const ())) ()  ex
