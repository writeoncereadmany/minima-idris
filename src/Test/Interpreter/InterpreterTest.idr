module InterpreterTest

import Minima.Interpreter.Interpreter
import Minima.Interpreter.Value
import Minima.AST
import Minima.Parsing.Parser
import Specdris.Spec
import Test.Support.EitherResults
import Lightyear.Strings
import Lightyear.Position
import Debug.Error

%language ElabReflection
%access export

plus : Value
plus = NativeFunction doPlus where
  doPlus : Implementation
  doPlus i [(NumberValue x), (NumberValue y)] = (NumberValue (x + y), i)
  doPlus _ args = error $ "Expected two numbers: got " ++ show args

print : Value
print = NativeFunction doPrint where
  doPrint : Implementation
  doPrint i [(StringValue x)] = (Success, i >>= (const $ putStr x))
  doPrint i [(NumberValue x)] = (Success, i >>= (const $ putStr $ show x))
  doPrint _ args = error $ "Expected a String or Number: got " ++ show args

prelude : InterpreterState
prelude = MkInterpreterState Success [("plus", plus), ("print", print)] (pure ())

evaluate : String -> Either String Value
evaluate text = do prog <- parse program text
                   let finalState = runProgram prelude prog
                   pure $ value finalState

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
