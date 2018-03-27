module CallTest

import Minima.Types.MinimaTypes
import Minima.Types.Bindings
import Minima.Types.TypeErrors
import Minima.Types.Call
import Specdris.Spec

%access export

errorsWith : (Show a) => (actual : Either TypeErrors a) -> (expected : TypeError) -> SpecResult
errorsWith (Left err) expected = err `shouldBe` [expected]
errorsWith (Right result) expected = UnaryFailure result $ "should have yielded type error" ++ show expected

returns : (Show a) => (actual : Either a MinimaType) -> (expected : MinimaType) -> SpecResult
returns (Right type) expected = type `shouldBe` expected
returns (Left errors) expected = UnaryFailure errors $ "should have yielded type " ++ show expected

string : MinimaType
string = Data (0, 0)

number : MinimaType
number = Data (0, 1)

nothing : MinimaType
nothing = Data (0, 2)

maybeString : MinimaType
maybeString = Union [string, nothing]

maybeNumber : MinimaType
maybeNumber = Union [number, nothing]

maybeStringOrNumber : MinimaType
maybeStringOrNumber = Union [string, number, nothing]

a : MinimaType
a = Named (0, 3)

b : MinimaType
b = Named (0, 4)

specs : IO ()
specs = spec $ do
  describe "Calls" $ do
    it "concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven" $ do
      [] |=> Function [string] string $? [string] `returns` string
    it "concreteFunctionCannotBeCalledWithWrongArgTypes" $ do
      [] |=> Function [string] string $? [number] `errorsWith` "Cannot assign (0, 1) to (0, 0)"
    it "parametricFunctionPopulatesReturnTypeFromArguments" $ do
      [] |=> Function [a] a $? [string] `returns` string
    it "parametricFunctionUnifiesMultipleArgsToSameNamedType" $ do
      [] |=> Function [a, a] a $? [string, nothing] `returns` maybeString
