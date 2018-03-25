module CallTest

import MinimaTypes
import Bindings
import TypeErrors
import Call
import test.Assertions

%access export

returns : (Show a) => MinimaType -> Either a MinimaType -> IO ()
returns = assertRight

yieldsTypeError : (Show a) => TypeError -> Either TypeErrors a -> IO ()
yieldsTypeError error = assertLeft [error]

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

concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven : IO ()
concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven =
    let fun = Function [string] string
     in returns string
        $ [] |=> fun $? [string]

concreteFunctionCannotBeCalledWithWrongArgTypes : IO ()
concreteFunctionCannotBeCalledWithWrongArgTypes =
    let fun = Function [string] string
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ [] |=> fun $? [number]

parametricFunctionPopulatesReturnTypeFromArguments : IO ()
parametricFunctionPopulatesReturnTypeFromArguments =
    let fun = Function [a] a
     in returns string
        $ [] |=> fun $? [string]

parametricFunctionUnifiesMultipleArgsToSameNamedType : IO ()
parametricFunctionUnifiesMultipleArgsToSameNamedType =
     let fun = Function [a, a] a
      in returns maybeString
         $ [] |=> fun $? [string, nothing]

cases : IO ()
cases = do putStrLn "  ** Test suite CallTest: "
           concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven
           concreteFunctionCannotBeCalledWithWrongArgTypes
           parametricFunctionPopulatesReturnTypeFromArguments
           parametricFunctionUnifiesMultipleArgsToSameNamedType
