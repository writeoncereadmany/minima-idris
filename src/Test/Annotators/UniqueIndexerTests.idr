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

-- we really don't care about asserting on the annotations -
-- just how names are dereferenced
Eq (Record [('Position, Position)]) where
  (==) a b = True

infixr 5 ~~
||| "is structurally identical to"
||| Asserts that the two programs yield structurally identical ASTs
||| after names have been anonymized by declaration order
(~~) : String -> String -> SpecResult
(~~) a b = let ast1 = parse program a >>= uniqueIndex
               ast2 = parse program b >>= uniqueIndex
            in ast1 `shouldBe` ast2

infixr 5 !~
||| "is not structurally identical to"
||| Asserts that the two programs do not yield structurally identical ASTs
||| after names have been anonymized by declaration order
(!~) : String -> String -> SpecResult
(!~) a b = let ast1 = parse program a >>= uniqueIndex
               ast2 = parse program b >>= uniqueIndex
            in ast1 `shouldNotBe` ast2

specs : IO ()
specs = spec $ do
  describe "Links variable to its declaration" $ do
    it "Just a declaration and access" $ do
      "a is 5, a" ~~ "b is 5, b"

    it "Can rename variables and yield structurally identical trees" $ do
      "a is 5, b is 6, a" ~~ "b is 5, a is 6, b"

    it "Structural differences in trees can be identified" $ do
      "a is 5, b is 6, a" !~ "b is 5, a is 6, a"

    it "Introduces new indices with new scopes" $ do
      "a is 5, (a, a is 6, a, [a] => a)[a]" ~~ "a is 5, (a, b is 6, b, [c] => c)[a]"

    it "Shadows previous declarations within a given scope" $ do
      "a is 5, a, a is 6, a, a is 1, a" ~~ "a is 5, a, b is 6, b, c is 1, c"
