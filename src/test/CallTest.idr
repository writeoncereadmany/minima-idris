module CallTest

import MinimaTypes
import Bindings
import TypeErrors
import Call
import test.Assertions

%access export

returns : (Show a) => MinimaType -> Either a MinimaType -> IO ()
returns = assertRight

string : MinimaType
string = Data (0, 0)

concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven : IO ()
concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven =
    let fun = Function [string] string
     in returns string
        $ [] |=> fun $? [string]

cases : IO ()
cases = do putStrLn "  ** Test suite CallTest: "
           concreteFunctionReturnsReturnTypeWhenCorrectArgsGiven
