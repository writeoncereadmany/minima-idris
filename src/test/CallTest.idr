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

cases : IO ()
cases = do putStrLn "  ** Test suite CallTest: "
           concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven
           concreteFunctionCannotBeCalledWithWrongArgTypes
