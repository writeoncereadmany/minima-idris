module AssignmentTest2

import test.Assertions
import MinimaTypes2
import Bindings2
import Assignment2
import TypeErrors

%access export

typechecks : (Show b) => Either TypeErrors b -> IO ()
typechecks = assertIsRight

yieldsTypeError : (Show b) => TypeError -> Either TypeErrors b -> IO ()
yieldsTypeError error = assertLeft [error]

yieldsTypeErrors : (Show b) => List TypeError -> Either TypeErrors b -> IO ()
yieldsTypeErrors errors = assertLeft errors

canAssignDataToItself : IO ()
canAssignDataToItself =
        typechecks
        $ [] |=> Data (0, 0) ->? Data (0, 0)

cannotAssignDataToOtherData : IO ()
cannotAssignDataToOtherData =
        yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ [] |=> Data (0, 0) ->? Data (0, 1)

canAssignDataToItsAlias : IO ()
canAssignDataToItsAlias =
    let string = Data (0, 0)
        prelude = [((1, 0), string)]
     in typechecks
        $ prelude |=> Bound (1, 0) ->? Data (0, 0)

assigningToUnboundBindsIt : IO ()
assigningToUnboundBindsIt =
    let string = Data (0, 0)
        a = Unbound (0, 1)
     in typechecks
        $ [] |=> string ->? a

canAssignMemberOfUnionToUnion : IO ()
canAssignMemberOfUnionToUnion =
    let string = Data (0, 0)
        nothing = Data (0, 1)
        maybeString = Union [string, nothing]
     in typechecks
        $ [] |=> string ->? maybeString

cannotAssignUnionToMemberOfUnion : IO ()
cannotAssignUnionToMemberOfUnion =
    let string = Data (0, 0)
        nothing = Data (0, 1)
        maybeString = Union [string, nothing]
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ [] |=> maybeString ->? string


canAssignUnionToLargerUnion : IO ()
canAssignUnionToLargerUnion =
    let string = Data (0, 0)
        number = Data (0, 1)
        nothing = Data (0, 2)
        maybeString = Union [string, nothing]
        maybeStringOrNumber = Union [string, number, nothing]
     in typechecks
        $ [] |=> maybeString ->? maybeStringOrNumber


cannotAssignUnionToSmallerUnion : IO ()
cannotAssignUnionToSmallerUnion =
    let string = Data (0, 0)
        number = Data (0, 1)
        nothing = Data (0, 2)
        maybeString = Union [string, nothing]
        maybeStringOrNumber = Union [string, number, nothing]
     in yieldsTypeErrors ["Cannot assign (0, 1) to (0, 0)", "Cannot assign (0, 1) to (0, 2)"]
        $ [] |=> maybeStringOrNumber ->? maybeString

canAssignFunctionToItself : IO ()
canAssignFunctionToItself =
    let number = Data (0, 0)
        plus = Function [number, number] number
     in typechecks
        $ [] |=> plus ->? plus

cannotAssignFunctionWhereArgumentsDiffer : IO ()
cannotAssignFunctionWhereArgumentsDiffer =
    let number = Data (0, 0)
        string = Data (0, 1)
        id = Function [number] number
        length = Function [string] number
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ [] |=> id ->? length

cannotAssignFunctionWhereReturnTypesDiffer : IO ()
cannotAssignFunctionWhereReturnTypesDiffer =
    let number = Data (0, 0)
        string = Data (0, 1)
        id = Function [number] number
        toString = Function [number] string
     in yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ [] |=> id ->? toString

canAssignFunctionWhereSourceArgSupertypeOfTargetArg : IO ()
canAssignFunctionWhereSourceArgSupertypeOfTargetArg =
    let string = Data (0, 0)
        nothing = Data (0, 1)
        maybeString = Union [string, nothing]
        id = Function [string] string
        maybeShow = Function [maybeString] string
     in typechecks
        $ [] |=> maybeShow ->? id

cannotAssignFunctionWhereSourceArgSubtypeOfTargetArg : IO ()
cannotAssignFunctionWhereSourceArgSubtypeOfTargetArg =
    let string = Data (0, 0)
        nothing = Data (0, 1)
        maybeString = Union [string, nothing]
        id = Function [string] string
        maybeShow = Function [maybeString] string
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ [] |=> id ->? maybeShow

canAssignFunctionWhereSourceReturnsSubtypeOfTargetReturn : IO ()
canAssignFunctionWhereSourceReturnsSubtypeOfTargetReturn =
    let string = Data (0, 0)
        nothing = Data (0, 1)
        maybeString = Union [string, nothing]
        id = Function [string] string
        verify = Function [string] maybeString
     in typechecks
        $ [] |=> id ->? verify


cannotAssignFunctionWhereSourceReturnsSupertypeOfTargetReturn : IO ()
cannotAssignFunctionWhereSourceReturnsSupertypeOfTargetReturn =
    let string = Data (0, 0)
        nothing = Data (0, 1)
        maybeString = Union [string, nothing]
        id = Function [string] string
        verify = Function [string] maybeString
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ [] |=> verify ->? id

cases : IO ()
cases = do putStrLn "  ** Test suite AssignmentTest2: "
           canAssignDataToItself
           cannotAssignDataToOtherData
           canAssignDataToItsAlias
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
