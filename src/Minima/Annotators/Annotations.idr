module Annotations

import Minima.AST
import Minima.MonadSemantics
import Control.Monad.State

%access public export

Simply : Type -> Type
Simply = State ()

mapAnnotationStringLiteral : (a -> b) -> a -> String -> Simply (Expression b i)
mapAnnotationStringLiteral f a value = pure $ StringLiteral (f a) value

mapAnnotationNumberLiteral : (a -> b) -> a -> Integer -> Simply (Expression b i)
mapAnnotationNumberLiteral f a value = pure $ NumberLiteral (f a) value

mapAnnotationVariable : (a -> b) -> a -> i -> Simply (Expression b i)
mapAnnotationVariable f a name = pure $ Variable (f a) name

mapAnnotationDefinition : (a -> b) -> a -> i -> Expression b i -> Simply (Expression b i)
mapAnnotationDefinition f a name value = pure $ Definition (f a) name value

mapAnnotationCall : (a -> b) -> a -> Expression b i -> List (Expression b i) -> Simply (Expression b i)
mapAnnotationCall f a fun args = pure $ Call (f a) fun args

mapAnnotationGroup : (a -> b) -> a -> List (Expression b i) -> Simply (Expression b i)
mapAnnotationGroup f a seq = pure $ Group (f a) seq

mutual
  mapAnnotationFunction : (a -> b) -> a -> List i -> Expression a i -> Simply (Expression b i)
  mapAnnotationFunction f a args body = do newBody <- foldExpression2 (annotate f) body
                                           pure $ Function (f a) args newBody

  annotate : (a -> b) -> ExpressionSemantics2 a i () (Expression b i)
  annotate f = MkExpressionSemantics
                (mapAnnotationStringLiteral f)
                (mapAnnotationNumberLiteral f)
                (mapAnnotationVariable f)
                (mapAnnotationDefinition f)
                (mapAnnotationFunction f)
                (mapAnnotationCall f)
                (mapAnnotationGroup f)

stripAnnotations : Expression a i -> Expression () i
stripAnnotations ex = evalState (foldExpression2 (annotate (const ())) ex) ()
