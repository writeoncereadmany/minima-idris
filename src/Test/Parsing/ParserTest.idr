module ParserTest

import Specdris.Spec
import Minima.AST
import Minima.Parsing.Parser
import Test.Support.EitherResults
import Lightyear.Strings
import Lightyear.Position
import Lightyear

%access export

cannot : (Show b) => Either a b -> SpecResult
cannot (Left _) = Success
cannot (Right r) = UnaryFailure r $ "Was not expecting to successfully parse"

pos : Position
pos = defaultPos Nothing

specs : IO ()
specs = spec $ do
  describe "Can parse individual expressions" $ do
    it "Cannot parse a naked bang" $ do
      cannot $ parse expression "!"
    it "Parse a variable" $ do
      parse expression "foo" \@/ Variable pos "foo"
    it "Parse a number" $ do
      parse expression "12" \@/ NumberLiteral pos 12
    it "Parse a string" $ do
      parse expression "'cheeseburger'" \@/ StringLiteral pos "cheeseburger"
    it "Parse a definition" $ do
      parse expression "foo is 12" \@/ Definition pos "foo" (NumberLiteral pos 12)
    it "Parse a function" $ do
      parse expression "[a, b] => 'shufflepants'" \@/ Function pos ["a", "b"] (StringLiteral pos "shufflepants")
    it "Parse a call" $ do
      parse expression "foo[bar]" \@/ Call pos (Variable pos "foo") [Variable pos "bar"]
    it "Parse a group" $ do
      parse expression "(12, 'hello', foo)" \@/ Group pos [NumberLiteral pos 12, StringLiteral pos "hello", Variable pos "foo"]
    it "Parse chained call" $ do
      parse expression "foo[bar][baz]" \@/ Call pos (Call pos (Variable pos "foo") [Variable pos "bar"]) [Variable pos "baz"]
    it "Can nest calls" $ do
      parse expression "foo[bar[baz]]" \@/ Call pos (Variable pos "foo") [Call pos (Variable pos "bar") [Variable pos "baz"]]


  describe "Can parse programs" $ do
    it "Can parse an empty program" $ do
      parse program "" \@/ []
    it "Can parse program with single expression" $ do
      parse program "foo" \@/ [Variable pos "foo"]
    it "Can parse program with single expression encased in whitespace" $ do
      parse program "    foo    " \@/ [Variable pos "foo"]
    it "Can parse program ending in newline" $ do
      parse program "print['Hello, World!']\n" \@/ [Call pos (Variable pos "print") [StringLiteral pos "Hello, World!"]]
    it "Cannot parse program with random crap at the end" $ do
      cannot $ parse program "foo bar"
    it "Can parse program with multiple expressions" $ do
      parse program "foo, bar" \@/ [Variable pos "foo", Variable pos "bar"]
    it "Can parse program with multiple expressions separated by whitespace" $ do
      parse program "   foo   ,\n  bar   " \@/ [Variable pos "foo", Variable pos "bar"]
