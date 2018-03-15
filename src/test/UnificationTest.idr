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
stringOrNumber = (0, 3)

stringOrNumberImpl : MinimaType
stringOrNumberImpl = Union [string, number]

stringOrBool : DeBruijnIndex
stringOrBool = (0, 4)

stringOrBoolImpl : MinimaType
stringOrBoolImpl = Union [string, bool]

stringOrBoolOrNumberImpl : MinimaType
stringOrBoolOrNumberImpl = Union [string, number, bool]

prelude : Bindings
prelude = [
  (string, stringImpl),
  (number, numberImpl),
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


cases : IO ()
cases = do putStrLn "  ** Test suite UnificationTest: "
           unifyingIdenticalPrimitivesYieldsThatPrimitive
           unifyingDifferentPrimitivesYieldsUnion
           unifyingIdenticalUnionsYieldsSameUnion
           unifyingOverlappingUnionsYieldsCompactRepresentation
