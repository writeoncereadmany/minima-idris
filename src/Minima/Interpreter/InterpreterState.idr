module InterpreterState

import Minima.Interpreter.DependentEnvironment
import Minima.Interpreter.Value
import Lens

%access public export

record InterpreterState (d : Nat) a i n where
  constructor MkInterpreterState
  _variables : Environment d n (Value a i n)
  _io : i

variables : Lens (InterpreterState d a i n) (Environment d n (Value a i n))
variables = lens _variables setVariables where
  setVariables : Environment d n (Value a i n) -> InterpreterState d a i n -> InterpreterState d a i n
  setVariables vs st = record { _variables = vs } st

enter : InterpreterState d a i n -> InterpreterState (S d) a i n
enter (MkInterpreterState vars io) = MkInterpreterState (enterScope vars) io

exit : InterpreterState (S d) a i n -> InterpreterState d a i n
exit (MkInterpreterState vars io) = MkInterpreterState (exitScope vars) io

io : Lens (InterpreterState d a i n) i
io = lens _io setIo where
  setIo : i -> InterpreterState d a i n -> InterpreterState d a i n
  setIo i st = record { _io = i } st
