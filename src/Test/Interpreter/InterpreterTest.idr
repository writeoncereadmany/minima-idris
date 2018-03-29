module InterpreterTest

import Minima.Interpreter.Interpreter
import Minima.Interpreter.Value
import Minima.AST
import Minima.Parsing.Parser
import Specdris.Spec
import Test.Support.EitherResults
import Lightyear.Strings
import Lightyear.Position

%access export

prelude : InterpreterState
prelude = MkInterpreterState Success [] (pure ())

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
