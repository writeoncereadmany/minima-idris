module MinimaPrelude

import Minima.AST
import Minima.Annotators.Annotations
import Minima.Parsing.Parser
import Minima.Interpreter.Environment
import Minima.Interpreter.Interpreter
import Minima.Interpreter.InterpreterState
import Minima.Interpreter.Interaction
import Lightyear.Strings
import Lightyear.Position
import Test.Support.MockInteraction
import Control.Monad.State
import Minima.Interpreter.Value
import Minima.Record
import Lens

plus : Value a io String
plus = NativeFunction doPlus where
  doPlus : Implementation a io String
  doPlus i [(NumberValue x), (NumberValue y)] = pure (NumberValue (x + y), i)
  doPlus _ args = Left $ "Expected two numbers: got " ++ show args

print : (Interaction i) => Value a (i ()) String
print = NativeFunction doPrint where
  doPrint : (Interaction i) => Implementation a (i ()) String
  doPrint i [(StringValue x)] = pure (Success, i >>= (const $ print x))
  doPrint i [(NumberValue x)] = pure (Success, i >>= (const $ print $ show x))
  doPrint _ args = Left $ "Expected a String or Number: got " ++ show args

prelude : (Interaction i) => InterpreterState a (i ()) String
prelude = MkInterpreterState [[("plus", plus), ("print", print)]] (pure ())

export
run : String -> State (InterpreterState () (MockInteraction ()) String) (Either String (Value () (MockInteraction ()) String))
run source = do let (Right prog) = parse program source | (Left error) => pure (Left error)
                runProgram (stripAnnotations prog)

export
evaluate : String -> Either String (Value () (MockInteraction ()) String)
evaluate source = evalState (run source) prelude

export
outputFrom : String -> Either String (List String)
outputFrom source = let finalState = execState (run source) prelude
                     in Right $ getOutput (io ^$ finalState)
