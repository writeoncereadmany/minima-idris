module UnificationTest

import test.Assertions
import MinimaTypes
import Bindings
import Unification

%access export

string : DeBruijnIndex
string = (0, 1)

stringImpl : MinimaType
stringImpl = Primitive string

number : DeBruijnIndex
number = (0, 2)

numberImpl : MinimaType
numberImpl = Primitive number

bool : DeBruijnIndex
bool = (0, 3)

boolImpl : MinimaType
boolImpl = Primitive bool

stringOrNumber : DeBruijnIndex
stringOrNumber = (0, 4)

stringOrNumberImpl : MinimaType
stringOrNumberImpl = Union [string, number]

stringOrBool : DeBruijnIndex
stringOrBool = (0, 5)

stringOrBoolImpl : MinimaType
stringOrBoolImpl = Union [string, bool]

stringOrBoolOrNumberImpl : MinimaType
stringOrBoolOrNumberImpl = Union [string, number, bool]

getString : DeBruijnIndex
getString = (0, 6)

getStringImpl : MinimaType
getStringImpl = Function [] string

getNumber : DeBruijnIndex
getNumber = (0, 7)

getNumberImpl : MinimaType
getNumberImpl = Function [] number

prelude : Bindings
prelude = [
  (string, stringImpl),
  (number, numberImpl),
  (bool, boolImpl),
  (stringOrNumber, stringOrNumberImpl),
  (stringOrBool, stringOrBoolImpl),
  (getString, getStringImpl),
  (getNumber, getNumberImpl)
  ]

unifyingIdenticalPrimitivesYieldsThatPrimitive : IO ()
unifyingIdenticalPrimitivesYieldsThatPrimitive =
  assertEq stringImpl
  $ prelude |=> string |? string

unifyingDifferentPrimitivesYieldsUnion : IO ()
unifyingDifferentPrimitivesYieldsUnion =
  assertEq stringOrNumberImpl
  $ prelude |=> string |? number

unifyingIdenticalUnionsYieldsSameUnion : IO ()
unifyingIdenticalUnionsYieldsSameUnion =
  assertEq stringOrNumberImpl
  $ prelude |=> stringOrNumber |? stringOrNumber

unifyingOverlappingUnionsYieldsCompactRepresentation : IO ()
unifyingOverlappingUnionsYieldsCompactRepresentation =
  assertEq stringOrBoolOrNumberImpl
  $ prelude |=> stringOrNumber |? stringOrBool

unifyingUnionAndPrimitiveYieldsUnion : IO ()
unifyingUnionAndPrimitiveYieldsUnion =
  assertEq stringOrBoolOrNumberImpl
  $ prelude |=> stringOrNumber |? bool

unifyingPrimitiveAndUnionYieldsUnion : IO ()
unifyingPrimitiveAndUnionYieldsUnion =
  assertEq stringOrBoolOrNumberImpl
  $ prelude |=> bool |? stringOrNumber

unifyingPrimitiveAndOverlappingUnionYieldsOriginalUnion : IO ()
unifyingPrimitiveAndOverlappingUnionYieldsOriginalUnion =
  assertEq stringOrNumberImpl
  $ prelude |=> string |? stringOrNumber

-- this is a big question: args/return types of functions are stored by their
-- de bruijn index. But if they're synthetic types, then (by definition) they
-- haven't already been defined, and don't have de bruijn indices.
-- if i synthesise a new type and then bind it, then i need access to the bindings
-- maybe i need to go back to old approach where bound type is one implementation
-- amongst others? 
unifyingFunctionTypesWithOverlappingArgsButDifferentReturnTypesUnionsReturnTypes : IO ()
unifyingFunctionTypesWithOverlappingArgsButDifferentReturnTypesUnionsReturnTypes =
  ?how_do_i_synthesise_return_types_on_the_fly

cases : IO ()
cases = do putStrLn "  ** Test suite UnificationTest: "
           unifyingIdenticalPrimitivesYieldsThatPrimitive
           unifyingDifferentPrimitivesYieldsUnion
           unifyingIdenticalUnionsYieldsSameUnion
           unifyingOverlappingUnionsYieldsCompactRepresentation
           unifyingPrimitiveAndUnionYieldsUnion
           unifyingUnionAndPrimitiveYieldsUnion
           unifyingPrimitiveAndOverlappingUnionYieldsOriginalUnion
