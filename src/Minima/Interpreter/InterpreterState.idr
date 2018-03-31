module InterpreterState

import Minima.Interpreter.Value
import Lens

%access public export

Environment : Type -> Type
Environment i = List (String, (Value i))

record InterpreterState i where
  constructor MkInterpreterState
  _value : Value i
  _variables : Environment i
  _io : i

value : Lens (InterpreterState i) (Value i)
value = lens _value setValue where
  setValue : Value i -> InterpreterState i -> InterpreterState i
  setValue v st = record { _value = v } st

variables : Lens (InterpreterState i) (Environment i)
variables = lens _variables setVariables where
  setVariables : Environment i -> InterpreterState i -> InterpreterState i
  setVariables vs st = record { _variables = vs } st

io : Lens (InterpreterState i) i
io = lens _io setIo where
  setIo : i -> InterpreterState i -> InterpreterState i
  setIo i st = record { _io = i } st

Show (InterpreterState i) where
  show x = show (value ^$ x)
