module UniqueIndexerTests

import Lightyear.Strings
import Lightyear.Position
import Lightyear

import Minima.AST
import Minima.Record
import Minima.Annotators.UniqueIndexer
import Minima.Parsing.Parser

import Specdris.Spec
import Test.Support.EitherResults

%access public export

none : Record [('Foo, Int)]
none = ('Foo := 1) :: []

Eq (Record [('Position, Position)]) where
  (==) a b = True

specs : IO ()
specs = spec $ do
  describe "Links variable to its declaration" $ do
    it "Just a declaration and access" $ do
      let prog1 = parse program "a is 5, a"
      let prog2 = parse program "b is 5, b"
      let indexed1 = prog1 >>= uniqueIndex
      let indexed2 = prog2 >>= uniqueIndex
      indexed1 `shouldBe` indexed2

    it "Can rename variables and yield structurally identical trees" $ do
      let prog1 = parse program "a is 5, b is 6, a"
      let prog2 = parse program "b is 5, a is 6, b"
      let indexed1 = prog1 >>= uniqueIndex
      let indexed2 = prog2 >>= uniqueIndex
      indexed1 `shouldBe` indexed2

    it "Structural differences in trees can be identified" $ do
      let prog1 = parse program "a is 5, b is 6, a"
      let prog2 = parse program "b is 5, a is 6, a"
      let indexed1 = prog1 >>= uniqueIndex
      let indexed2 = prog2 >>= uniqueIndex
      indexed1 `shouldNotBe` indexed2
