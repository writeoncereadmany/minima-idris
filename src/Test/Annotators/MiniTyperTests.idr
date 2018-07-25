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

    it "Two matching datatypes" $ do
      unify MString MString === MString
      unify MNumber MNumber === MNumber
      unify MSuccess MSuccess === MSuccess

    it "Unifying different data types yields a type error" $ do
      unify MString MNumber === MTypeError "Cannot unify String and Number"
      unify MSuccess MString === MTypeError "Cannot unify Success and String"
      unify MNumber MSuccess === MTypeError "Cannot unify Number and Success"

    it "Unifying any type with an Unbound yields that type" $ do
      unify MString (MUnbound 1) === MString
      unify (MUnbound 1) MSuccess === MSuccess

    it "Unifying with a pre-existing type error perpetuates that error" $ do
      unify (MTypeError "pre-existing") MString === MTypeError "pre-existing"
      unify MString (MTypeError "pre-existing") === MTypeError "pre-existing"
