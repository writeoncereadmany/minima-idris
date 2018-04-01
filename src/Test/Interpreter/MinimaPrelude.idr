module MinimaPrelude

import Minima.Interpreter.Interaction
import Minima.Interpreter.Interpreter
import Test.Support.MockInteraction
import Minima.Parsing.Parser
import Minima.AST
import Lightyear.Position
import Lightyear.Strings
import Lens

plus : Value a
plus = NativeFunction doPlus where
  doPlus : Implementation a
  doPlus i [(NumberValue x), (NumberValue y)] = pure (NumberValue (x + y), i)
  doPlus _ args = Left $ "Expected two numbers: got " ++ show args

doPrint : (Interaction i) => Implementation (i ())
doPrint i [(StringValue x)] = pure (Success, i >>= (const $ print x))
doPrint i [(NumberValue x)] = pure (Success, i >>= (const $ print $ show x))
doPrint _ args = Left $ "Expected a String or Number: got " ++ show args

print : (Interaction i) => Value (i ())
print = NativeFunction doPrint

prelude : (Interaction i) => InterpreterState (i ())
prelude = MkInterpreterState Success [("plus", plus), ("print", print)] (pure ())

export
run : String -> Either String (InterpreterState (MockInteraction ()))
run text = do prog <- parse program text
              runProgram prelude prog

export
evaluate : String -> Either String (Value (MockInteraction ()))
evaluate text = do result <- run text
                   pure $ value ^$ result

export
outputFrom : String -> Either String (List String)
outputFrom text = do result <- run text
                     pure $ getOutput (io ^$ result)
