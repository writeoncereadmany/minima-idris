module ParserTest

import Specdris.Spec
import Minima.AST
import Minima.Parsing.Parser
import Lightyear.Strings
import Lightyear

%access export

infixl 4 \@/
(\@/) : (Show a, Show b, Eq b) => Either a b -> b -> SpecResult
(\@/) (Left actual) expected = UnaryFailure actual $ "Got failure: expected success of " ++ show expected
(\@/) (Right actual) expected = actual === expected

infixl 4 >.<
(>.<) : (Show a, Show b, Eq a) => Either a b -> a -> SpecResult
(>.<) (Left actual) expected = actual === expected
(>.<) (Right actual) expected = UnaryFailure actual $ "Got success: expected failure of " ++ show expected

cannot : (Show b) => Either a b -> SpecResult
cannot (Left _) = Success
cannot (Right r) = UnaryFailure r $ "Was not expecting to successfully parse"

specs : IO ()
specs = spec $ do
  describe "Can parse individual expressions" $ do
    it "Cannot parse a naked bang" $ do
      cannot $ parse expression "!"
    it "Parse a variable" $ do
      parse expression "foo" \@/ Variable "foo"
    it "Parse a number" $ do
      parse expression "12" \@/ NumberLiteral 12
    it "Parse a string" $ do
      parse expression "'cheeseburger'" \@/ StringLiteral "cheeseburger"
    it "Parse a definition" $ do
      parse expression "foo is 12" \@/ Definition "foo" (NumberLiteral 12)
    it "Parse a function" $ do
      parse expression "[a, b] => 'shufflepants'" \@/ Function ["a", "b"] (StringLiteral "shufflepants")
    it "Parse a call" $ do
      parse expression "foo[bar]" \@/ Call (Variable "foo") [Variable "bar"]
