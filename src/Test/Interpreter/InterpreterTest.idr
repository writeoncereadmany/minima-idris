module InterpreterTest

import Test.Interpreter.MinimaPrelude
import Test.Support.EitherResults
import Specdris.Spec
import Minima.Interpreter.Value

%access export

specs: IO ()
specs = spec $ do
  describe "Evaluating simple values" $ do
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

  describe "Output" $ do
    it "Can generate program output" $ do
      outputFrom "print['Hello, World!']" \@/ ["Hello, World!"]
    it "Accumulates IO effects" $ do
      outputFrom "print['Hello, World!'], print['Goodbye, blue sky']" \@/ ["Hello, World!", "Goodbye, blue sky"]
    it "Accumulates IO effects from function arg evaluation" $ do
      outputFrom "a is [b, b] => print['three!'], a[print['one!'], print['two!']]" \@/ ["one!", "two!", "three!"]
    it "Accumulates IO effects from group evaluation" $ do
      outputFrom "(print['one'], print['two'])" \@/ ["one", "two"]
