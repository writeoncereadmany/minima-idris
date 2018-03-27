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

yieldsType' : (expected : MinimaType) -> (actual: MinimaType) -> SpecResult
yieldsType' = shouldBe

yieldsBindings' : (expected : Bindings) -> (actual: Bindings) -> SpecResult
yieldsBindings' = shouldBe

yieldsType : MinimaType -> MinimaType -> IO ()
yieldsType = assertEq

yieldsBindings : Bindings -> Bindings -> IO ()
yieldsBindings = assertEq

unifyingUnionAndSubsetYieldsThatUnion : IO ()
unifyingUnionAndSubsetYieldsThatUnion =
      yieldsType maybeString
      $ [] |=> string |? maybeString

unifyingOverlappingUnionsYieldsAllPossibleForms : IO ()
unifyingOverlappingUnionsYieldsAllPossibleForms =
      yieldsType maybeStringOrNumber
      $ [] |=> maybeString |? maybeNumber

unifyingDataAndFunctionsYieldsUnion : IO ()
unifyingDataAndFunctionsYieldsUnion =
      yieldsType stringOrShowNumber
      $ [] |=> string |? showNumber

unifyingFunctionsWithDifferentReturnTypesYieldsFunctionWithUnionReturnType : IO ()
unifyingFunctionsWithDifferentReturnTypesYieldsFunctionWithUnionReturnType =
      yieldsType (Function [number] stringOrNumber)
      $ [] |=> showNumber |? identity

unifyingFunctionsWithDifferentArgumentTypesYieldsIntersectionOfArgumentTypes : IO ()
unifyingFunctionsWithDifferentArgumentTypesYieldsIntersectionOfArgumentTypes =
      yieldsType (Function [string] string)
      $ [] |=> Function [maybeString] string |? Function [stringOrNumber] string

nonOverlappingBindingsCondenseToSameBindings : IO ()
nonOverlappingBindingsCondenseToSameBindings =
      yieldsBindings [((0, 0), string), ((0, 1), number)]
      $ condense [((0, 0), string), ((0, 1), number)]

multipleRedefinitionsCondenseToSingleDefinition : IO ()
multipleRedefinitionsCondenseToSingleDefinition =
      yieldsBindings [((0, 0), string)]
      $ condense [((0, 0), string), ((0, 0), string), ((0, 0), string)]

overlappingBindingsCondenseToUnificationOfBindings : IO ()
overlappingBindingsCondenseToUnificationOfBindings =
      yieldsBindings [((1, 0), maybeString)]
      $ condense [((1, 0), string), ((1, 0), nothing)]


specs : IO ()
specs = spec $ do
  describe "Basic unifications" $ do
    it "unifyingSameDatatypeYieldsThatDatatype" $ do
      yieldsType string
      $ [] |=> string |? string
    it "unifyingDifferentDatatypesYieldsTheirUnion" $ do
      yieldsType maybeString
      $ [] |=> string |? nothing

cases : IO ()
cases = do putStrLn "  ** Test suite UnificationTest: "
           unifyingUnionAndSubsetYieldsThatUnion
           unifyingOverlappingUnionsYieldsAllPossibleForms
           nonOverlappingBindingsCondenseToSameBindings
           overlappingBindingsCondenseToUnificationOfBindings
           multipleRedefinitionsCondenseToSingleDefinition
           unifyingDataAndFunctionsYieldsUnion
           unifyingFunctionsWithDifferentReturnTypesYieldsFunctionWithUnionReturnType
           unifyingFunctionsWithDifferentArgumentTypesYieldsIntersectionOfArgumentTypes
