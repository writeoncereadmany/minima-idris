module InterpreterState

import Minima.Interpreter.Value
import Lens

%access public export

Environment : Type -> Type
Environment i = List (String, (Value i))

record InterpreterState i where
  constructor MkInterpreterState
  value : Value i
  variables : Environment i
  io : i

lvalue : Lens (InterpreterState i) (Value i)
lvalue = lens value setValue where
  setValue : Value i -> InterpreterState i -> InterpreterState i
  setValue v st = record { value = v } st

lvariables : Lens (InterpreterState i) (Environment i)
lvariables = lens variables setVariables where
  setVariables : Environment i -> InterpreterState i -> InterpreterState i
  setVariables vs st = record { variables = vs } st

lio : Lens (InterpreterState i) i
lio = lens io setIo where
  setIo : i -> InterpreterState i -> InterpreterState i
  setIo i st = record { io = i } st

Show (InterpreterState i) where
  show x = show $ value x
