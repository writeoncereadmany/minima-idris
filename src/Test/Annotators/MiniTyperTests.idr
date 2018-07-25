module MiniTyperTests

import Lightyear.Strings
import Lightyear.Position
import Lightyear

import Minima.AST
import Minima.Record
import Minima.Annotators.MiniTyper
import Minima.Annotators.UniqueIndexer
import Minima.Parsing.Parser

import Specdris.Spec

%access public export

specs : IO ()
specs = spec $ do
  describe "Unifies two types" $ do
    it "Two strings" $ do
      unify MString MString === MString
    it "Two numbers" $ do
      unify MNumber MNumber === MNumber
    it "Unifying string and number yields a type error" $ do
      unify MString MNumber === MTypeError "Cannot unify String and Number"
    it "Unifying any type with an Unbound yields that type" $ do
      unify MString (MUnbound 1) === MString
    it "Unifying Unbound with any type yields that type" $ do
      unify (MUnbound 1) MSuccess === MSuccess
