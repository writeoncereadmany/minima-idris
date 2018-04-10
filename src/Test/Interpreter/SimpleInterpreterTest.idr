module SimpleInterpreterTest

import Minima.AST
import Minima.Parsing.Parser
import Minima.Interpreter.Environment
import Minima.Interpreter.SimpleInterpreter
import Minima.Interpreter.Interaction
import Lightyear.Strings
import Lightyear.Position
import Test.Support.EitherResults
import Test.Support.MockInteraction
import Specdris.Spec
import Control.Monad.State
import Minima.Interpreter.Value
import Minima.Record
import Lens

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
prelude = MkInterpreterState [[("plus", plus), ("print", print)]] (pure ())

run : String -> State (InterpreterState (MockInteraction ()) String) (Either String (Value (MockInteraction ()) String))
run source = do let (Right prog) = parse program source | (Left error) => pure (Left error)
                runProgram prog

evaluate : String -> Either String (Value (MockInteraction ()) String)
evaluate source = evalState (run source) prelude

outputFrom : String -> Either String (List String)
outputFrom source = let finalState = execState (run source) prelude
                     in Right $ getOutput (io ^$ finalState)

export
specs: IO ()
specs = spec $ do
  describe "Simple interpreter: Evaluating simple values" $ do
    it "Can evaluate a number" $ do
      evaluate "142" \@/ NumberValue 142
    it "Can evaluate a string" $ do
      evaluate "'stuffs'" \@/ StringValue "stuffs"
    it "Can store a value" $ do
      evaluate "a is 42, a" \@/ NumberValue 42
    it "Can store and call a function" $ do
      evaluate "a is [b] => b, a['hats']" \@/ StringValue "hats"
    it "Can evaluate a native function" $ do
      evaluate "plus[12, 42]" \@/ NumberValue 54
    it "Can evaluate a group" $ do
      evaluate "(1,2,3)" \@/ NumberValue 3
    it "Empty group evaluates to Success" $ do
      evaluate "()" \@/ Success

  describe "Simple intepreter: Output" $ do
    it "Can generate program output" $ do
      outputFrom "print['Hello, World!']" \@/ ["Hello, World!"]
    it "Accumulates IO effects" $ do
      outputFrom "print['Hello, World!'], print['Goodbye, blue sky']" \@/ ["Hello, World!", "Goodbye, blue sky"]
    it "Accumulates IO effects from function arg evaluation" $ do
      outputFrom "a is [b, b] => print['three!'], a[print['one!'], print['two!']]" \@/ ["one!", "two!", "three!"]
    it "Accumulates IO effects from group evaluation" $ do
      outputFrom "(print['one'], print['two'])" \@/ ["one", "two"]
