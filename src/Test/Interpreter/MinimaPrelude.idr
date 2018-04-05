module MinimaPrelude

import Minima.Interpreter.Interaction
import Minima.Interpreter.Interpreter
import Test.Support.MockInteraction
import Minima.Parsing.Parser
import Minima.AST
import Minima.Whatever
import Minima.Record
import Lightyear.Position
import Lightyear.Strings
import Lens

Whatever String where
  whatever = ""

plus : Value a String
plus = NativeFunction doPlus where
  doPlus : Implementation a String
  doPlus i [(NumberValue x), (NumberValue y)] = pure (NumberValue (x + y), i)
  doPlus _ args = Left $ "Expected two numbers: got " ++ show args

print : (Interaction i) => Value (i ()) String
print = NativeFunction doPrint where
  doPrint : (Interaction i) => Implementation (i ()) String
  doPrint i [(StringValue x)] = pure (Success, i >>= (const $ print x))
  doPrint i [(NumberValue x)] = pure (Success, i >>= (const $ print $ show x))
  doPrint _ args = Left $ "Expected a String or Number: got " ++ show args

prelude : (Interaction i) => InterpreterState (i ()) String
prelude = MkInterpreterState Success [("plus", plus), ("print", print)] (pure ())

export
run : String -> Either String (InterpreterState (MockInteraction ()) String)
run text = do prog <- parse program text
              runProgram prelude prog

export
evaluate : String -> Either String (Value (MockInteraction ()) String)
evaluate text = do result <- run text
                   pure $ value ^$ result

export
outputFrom : String -> Either String (List String)
outputFrom text = do result <- run text
                     pure $ getOutput (io ^$ result)
