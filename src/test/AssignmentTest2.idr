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

cases : IO ()
cases = do putStrLn "  ** Test suite AssignmentTest2: "
           canAssignDataToItself
           cannotAssignDataToOtherData
           canAssignDataToItsAlias
           assigningToUnboundBindsIt
