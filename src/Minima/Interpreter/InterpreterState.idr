module InterpreterState

import Minima.Interpreter.Environment
import Minima.Interpreter.Value
import Lens

%access public export

record InterpreterState a i n where
  constructor MkInterpreterState
  _variables : Environment n (Value a i n)
  _io : i

variables : Lens (InterpreterState a i n) (Environment n (Value a i n))
variables = lens _variables setVariables where
  setVariables : Environment n (Value a i n) -> InterpreterState a i n -> InterpreterState a i n
  setVariables vs st = record { _variables = vs } st

io : Lens (InterpreterState a i n) i
io = lens _io setIo where
  setIo : i -> InterpreterState a i n -> InterpreterState a i n
  setIo i st = record { _io = i } st
