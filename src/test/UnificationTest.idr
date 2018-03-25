module UnificationTest

import test.Assertions
import MinimaTypes
import TypeErrors
import Bindings
import Unification

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

yieldsType : (Show a) => MinimaType -> Either a MinimaType -> IO ()
yieldsType = assertRight

yieldsBindings : (Show a) => Bindings -> Either a Bindings -> IO ()
yieldsBindings = assertRight

unifyingSameDatatypeYieldsThatDatatype : IO ()
unifyingSameDatatypeYieldsThatDatatype =
      yieldsType string
      $ [] |=> string |? string

unifyingDifferentDatatypesYieldsTheirUnion : IO ()
unifyingDifferentDatatypesYieldsTheirUnion =
      yieldsType maybeString
      $ [] |=> string |? nothing

unifyingUnionAndSubsetYieldsThatUnion : IO ()
unifyingUnionAndSubsetYieldsThatUnion =
      yieldsType maybeString
      $ [] |=> string |? maybeString

unifyingOverlappingUnionsYieldsAllPossibleForms : IO ()
unifyingOverlappingUnionsYieldsAllPossibleForms =
      yieldsType maybeStringOrNumber
      $ [] |=> maybeString |? maybeNumber

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

cases : IO ()
cases = do putStrLn "  ** Test suite UnificationTest: "
           unifyingSameDatatypeYieldsThatDatatype
           unifyingDifferentDatatypesYieldsTheirUnion
           unifyingUnionAndSubsetYieldsThatUnion
           unifyingOverlappingUnionsYieldsAllPossibleForms
           nonOverlappingBindingsCondenseToSameBindings
           overlappingBindingsCondenseToUnificationOfBindings
           multipleRedefinitionsCondenseToSingleDefinition
