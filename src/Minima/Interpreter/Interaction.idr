module Interaction

import Control.Monad.Writer

%access public export

interface Monad m => Interaction (m : Type -> Type) where
  print : String -> m ()

Interaction IO where
  print = putStr
