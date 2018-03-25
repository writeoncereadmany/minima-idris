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
  call b (Named loc) args = b |=> lookupType loc b $? args
  call b (Function params returns) args = case assignArgs b params args of
    (Left typeErrors) => Left typeErrors
    (Right bindings) => Right $ resolveType returns $ mergeBindings b bindings
  call b notfun _ = Left ["Cannot call " ++ show notfun]
