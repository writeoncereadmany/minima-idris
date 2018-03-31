module MockInteraction

import Minima.Interpreter.Interaction

%access public export

data MockInteraction a = Mock a (List String)

Functor MockInteraction where
  map f (Mock a output) = Mock (f a) output

Applicative MockInteraction where
  pure a = Mock a []
  (<*>) (Mock f fout) (Mock a aout) = Mock (f a) (fout ++ aout)

Monad MockInteraction where
  (>>=) (Mock a aout) f = case f a of
    (Mock b bout) => Mock b (aout ++ bout)

Interaction MockInteraction where
  print x = Mock () [x]

getOutput : MockInteraction a -> List String
getOutput (Mock payload output) = output
