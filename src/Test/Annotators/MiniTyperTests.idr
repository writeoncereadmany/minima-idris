module MiniTyperTests

import Lightyear.Strings
import Lightyear.Position
import Lightyear

import Minima.AST
import Minima.Record
import Minima.Annotators.MiniTyper
import Minima.Annotators.MTypes
import Minima.Annotators.UniqueIndexer
import Minima.Parsing.Parser
import Test.Support.EitherResults

import Specdris.Spec

%access public export

specs : IO ()
specs = spec $ do
  describe "Unifies two types" $ do

    it "Two matching datatypes" $ do
      unify [] MString MString \@/ ([], MString)
      unify [] MNumber MNumber \@/ ([], MNumber)
      unify [] MSuccess MSuccess \@/ ([], MSuccess)

    it "Unifying different data types yields a type error" $ do
      unify [] MString MNumber >.< MkTypeError "Cannot unify String and Number"
      unify [] MSuccess MString >.< MkTypeError "Cannot unify Success and String"
      unify [] MNumber MSuccess >.< MkTypeError "Cannot unify Number and Success"

    it "Unifying any type with an Unbound yields that type" $ do
      unify [] MString (MUnbound 1) \@/ ([(1, MString)], MString)
      unify [] (MUnbound 1) MSuccess \@/ ([(1, MSuccess)], MSuccess)

    it "Concrete function unifies with itself" $ do
      let fun = MFunction [MNumber, MNumber] MNumber
      unify [] fun fun \@/ ([], fun)

    it "Functions with different arities do not unify" $ do
      let oneArg = MFunction [MNumber] MString
      let noArgs = MFunction [] MString
      unify [] oneArg noArgs >.< MkTypeError "Arity mismatch"

    it "Functions with different concrete argument types do not unify" $ do
      let showNum = MFunction [MNumber] MString
      let showSuccess = MFunction [MSuccess] MString
      unify [] showNum showSuccess >.< MkTypeError "Cannot unify Number and Success"

    it "Functions with different return types do not unify" $ do
      let increment = MFunction [MNumber] MNumber
      let showNumber = MFunction [MNumber] MString
      unify [] increment showNumber >.< MkTypeError "Cannot unify Number and String"

    it "Functions with unbound arguments get bound when unifying with concrete functions" $ do
      let generic = MFunction [MUnbound 1] (MUnbound 2)
      let concrete = MFunction [MNumber] MString
      unify [] generic concrete \@/ ([], concrete)
      unify [] concrete generic \@/ ([], concrete)

    it "Bindings carry over across multiple references to the same unbound" $ do
      let invocation1 = MFunction [MNumber] (MUnbound 4)
      let invocation2 = MFunction [MUnbound 3] MNumber
      let generic = MFunction [MUnbound 1] (MUnbound 1)
      let expected = MFunction [MNumber] MNumber
      unify [] invocation1 generic \@/ ([], expected)
      unify [] invocation2 generic \@/ ([], expected)
      unify [] generic invocation1 \@/ ([], expected)
