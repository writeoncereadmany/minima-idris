module Call

import MinimaTypes
import Bindings
import TypeErrors
import Assignment

%access public export

data CallOp = Call MinimaType (List MinimaType)
infixl 8 $?
($?) : MinimaType -> (List MinimaType) -> CallOp
($?) = Call

mutual
  WithBindings CallOp (Either TypeErrors MinimaType) where
    (|=>) bindings (Call fun args) = call bindings fun args

  call : Bindings -> MinimaType -> List MinimaType -> Either TypeErrors MinimaType
  call b fun args = Right fun
