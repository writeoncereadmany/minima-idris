module UniqueIndexerTests

import Specdris.Spec
import Minima.AST
import Test.Support.EitherResults
import Minima.Record
import Minima.Annotators.UniqueIndexer

%access public export

none : Record [('Foo, Int)]
none = ('Foo := 1) :: []

Eq (Record [('Foo, Int)]) where
  (==) a b = True

specs : IO ()
specs = spec $ do
  describe "Links variable to its declaration" $ do
    it "Just a declaration and access" $ do
      let program = Group none [Definition none "a" (NumberLiteral none 3), Variable none "a"]
      let actual = uniqueIndex program
      let expected = Group none [Definition none 0 (NumberLiteral none 3), Variable none 0]
      actual \@/ expected
