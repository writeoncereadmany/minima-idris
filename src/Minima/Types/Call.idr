module Call

import Minima.Types.MinimaTypes
import Minima.Types.Bindings
import Minima.Types.TypeErrors
import Minima.Types.Assignment
import Minima.Types.Unification

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
    (Right bindings) => Right $ resolveType returns $ unifyBindings b bindings
  call b notfun _ = Left ["Cannot call " ++ show notfun]
