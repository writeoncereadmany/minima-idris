module Annotations

import Minima.AST

%access public export

mapAnnotations : (a -> b) -> Expression a i -> Expression b i
mapAnnotations f (StringLiteral a text) = StringLiteral (f a) text
mapAnnotations f (NumberLiteral a number) = NumberLiteral (f a) number
mapAnnotations f (Variable a name) = Variable (f a) name
mapAnnotations f (Definition a name value) = Definition (f a) name (mapAnnotations f value)
mapAnnotations f (Function a params body) = Function (f a) params (mapAnnotations f body)
mapAnnotations f (Call a fun args) = Call (f a) (mapAnnotations f fun) (mapAnnotations f <$> args)
mapAnnotations f (Group a seq) = Group (f a) (mapAnnotations f <$> seq)

stripAnnotations : Expression a i -> Expression () i
stripAnnotations ex = mapAnnotations (const ()) ex
