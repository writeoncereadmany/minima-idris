module MiniTyperTests

import Lightyear.Strings
import Lightyear.Position
import Lightyear

import Minima.Parsing.Parser
import Minima.AST
import Minima.Record

import Minima.Annotators.MiniTyper
import Minima.Annotators.MTypes
import Minima.Annotators.UniqueIndexer
import Minima.Annotators.Bindings
import Test.Support.EitherResults

import Specdris.Spec

%access public export

typeWith : Bindings -> String -> Either String MType
typeWith bindings source = do
  ast <- parse program source
  indexed <- uniqueIndex ast
  typed <- typeExpWith bindings indexed
  pure $ typeOf typed

type : String -> Either String MType
type = typeWith []

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
      unify [] MString (MUnbound 1) \@/ ([Bound [1] MString], MString)
      unify [] (MUnbound 1) MSuccess \@/ ([Bound [1] MSuccess], MSuccess)

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
      unify [] generic invocation2 \@/ ([], expected)

  describe "Annotates an expression tree with appropriate types" $ do
    it "Assigns appropriate types to Strings" $ do
      type "'Hello!'" \@/ MString
    it "Assigns appropriate types to Numbers" $ do
      type "42" \@/ MNumber
    it "Remembers the type of a variable" $ do
      type "x is 42, x" \@/ MNumber
    it "Rejects programs which throw away results" $ do
      type "42, 69" >.< "Type Error: Return values of non-terminal group expressions must not be ignored: ignoring a Number"
    it "Works out return type of function from its parameters" $ do
      type "fun is [a, b] => a, fun[12, 'Hello']" \@/ MNumber
 
