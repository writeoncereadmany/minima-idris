module SimpleInterpreterState

import Minima.Interpreter.Environment
import Minima.Interpreter.Value
import Lens

%access public export

record InterpreterState i n where
  constructor MkInterpreterState
  _variables : Environment n (Value i n)
  _io : i

variables : Lens (InterpreterState i n) (Environment n (Value i n))
variables = lens _variables setVariables where
  setVariables : Environment n (Value i n) -> InterpreterState i n -> InterpreterState i n
  setVariables vs st = record { _variables = vs } st

io : Lens (InterpreterState i n) i
io = lens _io setIo where
  setIo : i -> InterpreterState i n -> InterpreterState i n
  setIo i st = record { _io = i } st
