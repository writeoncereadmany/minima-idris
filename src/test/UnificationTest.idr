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

prelude : Bindings
prelude = [
  (string, stringImpl),
  (number, numberImpl),
  (bool, boolImpl),
  (stringOrNumber, stringOrNumberImpl),
  (stringOrBool, stringOrBoolImpl)
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


cases : IO ()
cases = do putStrLn "  ** Test suite UnificationTest: "
           unifyingIdenticalPrimitivesYieldsThatPrimitive
           unifyingDifferentPrimitivesYieldsUnion
           unifyingIdenticalUnionsYieldsSameUnion
           unifyingOverlappingUnionsYieldsCompactRepresentation
           unifyingPrimitiveAndUnionYieldsUnion
           unifyingUnionAndPrimitiveYieldsUnion
           unifyingPrimitiveAndOverlappingUnionYieldsOriginalUnion
