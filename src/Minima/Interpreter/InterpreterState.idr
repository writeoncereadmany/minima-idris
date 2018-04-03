module InterpreterState

import Minima.Interpreter.Value
import Lens

%access public export

Environment : Type -> Type -> Type
Environment i n = List (n, (Value i n))

record InterpreterState i n where
  constructor MkInterpreterState
  _value : Value i n
  _variables : Environment i n
  _io : i

value : Lens (InterpreterState i n) (Value i n)
value = lens _value setValue where
  setValue : Value i n -> InterpreterState i n -> InterpreterState i n
  setValue v st = record { _value = v } st

variables : Lens (InterpreterState i n) (Environment i n)
variables = lens _variables setVariables where
  setVariables : Environment i n -> InterpreterState i n -> InterpreterState i n
  setVariables vs st = record { _variables = vs } st

io : Lens (InterpreterState i n) i
io = lens _io setIo where
  setIo : i -> InterpreterState i n -> InterpreterState i n
  setIo i st = record { _io = i } st

Show n => Show (InterpreterState i n) where
  show x = show (value ^$ x)
