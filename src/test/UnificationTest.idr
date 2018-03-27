module UnificationTest

import test.Assertions
import MinimaTypes
import TypeErrors
import Bindings
import Unification
import Specdris.Spec

%access export

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

stringOrNumber : MinimaType
stringOrNumber = Union [string, number]

showNumber : MinimaType
showNumber = Function [number] string

identity : MinimaType
identity = Function [number] number

stringOrShowNumber : MinimaType
stringOrShowNumber = Union [string, showNumber]

infixl 1 :::
(:::) : (actual : MinimaType) -> (expected: MinimaType) -> SpecResult
(:::) = shouldBe

specs : IO ()
specs = spec $ do
  describe "Unifying types:" $ do
    it "unifyingSameDatatypeYieldsThatDatatype" $ do
      [] |=> string |? string ::: string
    it "unifyingDifferentDatatypesYieldsTheirUnion" $ do
      [] |=> string |? nothing ::: maybeString
    it "unifyingUnionAndSubsetYieldsThatUnion" $ do
      [] |=> string |? maybeString ::: maybeString
    it "unifyingOverlappingUnionsYieldsAllPossibleForms" $ do
      [] |=> maybeString |? maybeNumber ::: maybeStringOrNumber
    it "unifyingDataAndFunctionsYieldsUnion" $ do
      [] |=> string |? showNumber ::: stringOrShowNumber
    it "unifyingFunctionsWithDifferentReturnTypesYieldsFunctionWithUnionReturnType" $ do
      [] |=> showNumber |? identity ::: Function [number] stringOrNumber
    it "unifyingFunctionsWithDifferentArgumentTypesYieldsIntersectionOfArgumentTypes" $ do
      [] |=> Function [maybeString] string |? Function [stringOrNumber] string ::: Function [string] string

  describe "Unifying bindings: " $ do
    it "nonOverlappingBindingsCondenseToSameBindings" $ do
      condense [((0, 0), string), ((0, 1), number)] === [((0, 0), string), ((0, 1), number)]
    it "multipleRedefinitionsCondenseToSingleDefinition" $ do
      condense [((0, 0), string), ((0, 0), string), ((0, 0), string)] === [((0,0), string)]
    it "overlappingBindingsCondenseToUnificationOfBindings" $ do
      condense [((1, 0), string), ((1, 0), nothing)] === [((1, 0), maybeString)]
