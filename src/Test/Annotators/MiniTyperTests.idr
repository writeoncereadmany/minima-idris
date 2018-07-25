module MiniTyperTests

import Lightyear.Strings
import Lightyear.Position
import Lightyear

import Minima.AST
import Minima.Record
import Minima.Annotators.MiniTyper
import Minima.Annotators.UniqueIndexer
import Minima.Parsing.Parser
import Test.Support.EitherResults

import Specdris.Spec

%access public export

specs : IO ()
specs = spec $ do
  describe "Unifies two types" $ do

    it "Two matching datatypes" $ do
      unify MString MString \@/ MString
      unify MNumber MNumber \@/ MNumber
      unify MSuccess MSuccess \@/ MSuccess

    it "Unifying different data types yields a type error" $ do
      unify MString MNumber >.< MkTypeError "Cannot unify String and Number"
      unify MSuccess MString >.< MkTypeError "Cannot unify Success and String"
      unify MNumber MSuccess >.< MkTypeError "Cannot unify Number and Success"

    it "Unifying any type with an Unbound yields that type" $ do
      unify MString (MUnbound 1) \@/ MString
      unify (MUnbound 1) MSuccess \@/ MSuccess

    it "Concrete function unifies with itself" $ do
      let fun = MFunction [MNumber, MNumber] MNumber
      unify fun fun \@/ fun

    it "Functions with different arities do not unify" $ do
      let oneArg = MFunction [MNumber] MString
      let noArgs = MFunction [] MString
      unify oneArg noArgs >.< MkTypeError "Arity mismatch: Cannot unify [Number] -> String and [] -> String"

    it "Functions with different concrete argument types do not unify" $ do
      let showNum = MFunction [MNumber] MString
      let showSuccess = MFunction [MSuccess] MString
      unify showNum showSuccess >.< MkTypeError "Cannot unify Number and Success"
