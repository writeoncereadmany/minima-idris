module AssignmentTest

import test.Assertions
import MinimaTypes
import Bindings
import Assignment
import TypeErrors

%access export

typechecks : (Show b) => Either TypeErrors b -> IO ()
typechecks = assertIsRight

yieldsTypeError : (Show b) => TypeError -> Either TypeErrors b -> IO ()
yieldsTypeError error = assertLeft [error]

yieldsTypeErrors : (Show b) => List TypeError -> Either TypeErrors b -> IO ()
yieldsTypeErrors errors = assertLeft errors

yieldsBindings : (Show a) => Bindings -> Either a Bindings -> IO ()
yieldsBindings bindings = assertRight bindings

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

canAssignDataToItself : IO ()
canAssignDataToItself =
        typechecks
        $ [] |=> string ->? string

cannotAssignDataToOtherData : IO ()
cannotAssignDataToOtherData =
        yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ [] |=> string ->? number

canAssignDataToItsAlias : IO ()
canAssignDataToItsAlias =
    let text = Named (1, 0)
        bindings = [((1, 0), string)]
     in typechecks
        $ bindings |=> string ->? text

canAssignAliasToItsRootType : IO ()
canAssignAliasToItsRootType =
    let text = Named (1, 0)
        bindings = [((1, 0), string)]
     in typechecks
        $ bindings |=> text ->? string

assigningToUnboundBindsIt : IO ()
assigningToUnboundBindsIt =
    let param = Named (1, 0)
     in yieldsBindings [((1, 0), string)]
        $ [] |=> string ->? param

canAssignMemberOfUnionToUnion : IO ()
canAssignMemberOfUnionToUnion =
        typechecks
        $ [] |=> string ->? maybeString

cannotAssignUnionToMemberOfUnion : IO ()
cannotAssignUnionToMemberOfUnion =
        yieldsTypeError "Cannot assign (0, 2) to (0, 0)"
        $ [] |=> maybeString ->? string

canAssignUnionToLargerUnion : IO ()
canAssignUnionToLargerUnion =
        typechecks
        $ [] |=> maybeString ->? maybeStringOrNumber


cannotAssignUnionToSmallerUnion : IO ()
cannotAssignUnionToSmallerUnion =
        yieldsTypeErrors ["Cannot assign (0, 1) to (0, 0)", "Cannot assign (0, 1) to (0, 2)"]
        $ [] |=> maybeStringOrNumber ->? maybeString

canAssignFunctionToItself : IO ()
canAssignFunctionToItself =
    let plus = Function [number, number] number
     in typechecks
        $ [] |=> plus ->? plus

cannotAssignFunctionWhereArgumentsDiffer : IO ()
cannotAssignFunctionWhereArgumentsDiffer =
    let id = Function [number] number
        length = Function [string] number
     in yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ [] |=> id ->? length

cannotAssignFunctionWhereReturnTypesDiffer : IO ()
cannotAssignFunctionWhereReturnTypesDiffer =
    let id = Function [number] number
        toString = Function [number] string
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ [] |=> id ->? toString

canAssignFunctionWhereSourceArgSupertypeOfTargetArg : IO ()
canAssignFunctionWhereSourceArgSupertypeOfTargetArg =
    let id = Function [string] string
        maybeShow = Function [maybeString] string
     in typechecks
        $ [] |=> maybeShow ->? id

cannotAssignFunctionWhereSourceArgSubtypeOfTargetArg : IO ()
cannotAssignFunctionWhereSourceArgSubtypeOfTargetArg =
    let id = Function [string] string
        maybeShow = Function [maybeString] string
     in yieldsTypeError "Cannot assign (0, 2) to (0, 0)"
        $ [] |=> id ->? maybeShow

canAssignFunctionWhereSourceReturnsSubtypeOfTargetReturn : IO ()
canAssignFunctionWhereSourceReturnsSubtypeOfTargetReturn =
    let id = Function [string] string
        verify = Function [string] maybeString
     in typechecks
        $ [] |=> id ->? verify


cannotAssignFunctionWhereSourceReturnsSupertypeOfTargetReturn : IO ()
cannotAssignFunctionWhereSourceReturnsSupertypeOfTargetReturn =
    let id = Function [string] string
        verify = Function [string] maybeString
     in yieldsTypeError "Cannot assign (0, 2) to (0, 0)"
        $ [] |=> verify ->? id

canAssignGenericFunctionToSpecificFunctionIfUnboundArgsLineUp : IO ()
canAssignGenericFunctionToSpecificFunctionIfUnboundArgsLineUp =
    let concrete = Function [string, number] number
        parametric = Function [a, b] b
     in typechecks
        $ [] |=> parametric ->? concrete

cannotAssignGenericFunctionToSpecificFunctionIfUnboundArgsDoNotLineUp : IO ()
cannotAssignGenericFunctionToSpecificFunctionIfUnboundArgsDoNotLineUp =
    let concrete = Function [string, number] number
        parametric = Function [a, b] a
     in yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ [] |=> parametric ->? concrete

assigningArgsYieldsNewBindings : IO ()
assigningArgsYieldsNewBindings =
        yieldsBindings [((0, 3), string)]
        $ assignArgs [] [a] [string]

cases : IO ()
cases = do putStrLn "  ** Test suite AssignmentTest: "
           canAssignDataToItself
           cannotAssignDataToOtherData
           canAssignDataToItsAlias
           canAssignAliasToItsRootType
           assigningToUnboundBindsIt
           canAssignMemberOfUnionToUnion
           cannotAssignUnionToMemberOfUnion
           canAssignUnionToLargerUnion
           cannotAssignUnionToSmallerUnion
           canAssignFunctionToItself
           cannotAssignFunctionWhereArgumentsDiffer
           cannotAssignFunctionWhereReturnTypesDiffer
           canAssignFunctionWhereSourceArgSupertypeOfTargetArg
           cannotAssignFunctionWhereSourceArgSubtypeOfTargetArg
           canAssignFunctionWhereSourceReturnsSubtypeOfTargetReturn
           cannotAssignFunctionWhereSourceReturnsSupertypeOfTargetReturn
           canAssignGenericFunctionToSpecificFunctionIfUnboundArgsLineUp
           cannotAssignGenericFunctionToSpecificFunctionIfUnboundArgsDoNotLineUp
           assigningArgsYieldsNewBindings
