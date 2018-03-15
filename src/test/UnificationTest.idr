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

stringOrNumber : DeBruijnIndex
stringOrNumber = (0, 3)

stringOrNumberImpl : MinimaType
stringOrNumberImpl = Union [string, number]

prelude : Bindings
prelude = [
  (string, stringImpl),
  (number, numberImpl),
  (stringOrNumber, stringOrNumberImpl)
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

cases : IO ()
cases = do putStrLn "  ** Test suite UnificationTest: "
           unifyingIdenticalPrimitivesYieldsThatPrimitive
           unifyingDifferentPrimitivesYieldsUnion
           unifyingIdenticalUnionsYieldsSameUnion
