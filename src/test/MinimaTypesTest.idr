module MinimaTypesTest

import MinimaTypes
import test.Assertions

%access export

typechecks : (Show a) => Maybe a -> IO ()
typechecks = assertNothing

yieldsTypeError : (Show a, Eq a) => a -> Maybe a -> IO ()
yieldsTypeError = assertJust

yieldsType : (Show a, Show b, Eq b) => b -> Either a b -> IO ()
yieldsType = assertRight

string : MinimaType
string = Primitive "String"

number : MinimaType
number = Primitive "Number"

text : MinimaType
text = NamedType "Text"

textAlias : (String, MinimaType)
textAlias = ("Text", string)

canAssignTypeToItself : IO ()
canAssignTypeToItself = typechecks
        $ [] =>> string ->? string

cannotAssignTypeToADifferentPrimitive : IO ()
cannotAssignTypeToADifferentPrimitive = yieldsTypeError "Cannot assign String to Number"
        $ [] =>> string ->? number

canAssignTypeToItsOwnAlias : IO ()
canAssignTypeToItsOwnAlias = typechecks
        $ [textAlias] =>> string ->? text

canAssignAliasToItsResolution : IO ()
canAssignAliasToItsResolution = typechecks
        $ [textAlias] =>> text ->? string

invalidSourceAliasIsATypeError : IO ()
invalidSourceAliasIsATypeError = yieldsTypeError "No such type Text found"
        $ [] =>> text ->? string

invalidTargetAliasIsATypeError : IO ()
invalidTargetAliasIsATypeError = yieldsTypeError "No such type Text found"
        $ [] =>> string ->? text

canAssignFunctionToItself : IO ()
canAssignFunctionToItself = typechecks
        $ [] =>> (Function [] string) ->? (Function [] string)

cannotAssignFunctionToOneReturningADifferentType : IO ()
cannotAssignFunctionToOneReturningADifferentType =
        yieldsTypeError "Cannot assign String to Number"
        $ [] =>> (Function [] string) ->? (Function [] number)

cannotAssignFunctionToOneTakingADifferentType : IO ()
cannotAssignFunctionToOneTakingADifferentType =
        yieldsTypeError "Cannot assign Number to String"
        $ [] =>> (Function [string] string) ->? (Function [number] string)

cannotAssignFunctionToOneOfDifferentArity : IO ()
cannotAssignFunctionToOneOfDifferentArity =
        yieldsTypeError "Arity mismatch"
        $ [] =>> (Function [string] string) ->? (Function [] string)

cannotAssignFunctionToPrimitive : IO ()
cannotAssignFunctionToPrimitive =
        yieldsTypeError "Cannot assign [] => String to String"
        $ [] =>> (Function [] string) ->? string

callingAFunctionWithCorrectParamsYieldsReturnType : IO ()
callingAFunctionWithCorrectParamsYieldsReturnType =
        yieldsType string
        $ [] =>> (Function [] string) $? []

cannotCallFunctionWithArgumentsOfDifferentTypes : IO ()
cannotCallFunctionWithArgumentsOfDifferentTypes =
        assertLeft "Cannot assign Number to String"
        $ [] =>> (Function [string] string) $? [number]

cannotCallFunctionWithWrongNumberOfArguments : IO ()
cannotCallFunctionWithWrongNumberOfArguments =
        assertLeft "Arity mismatch: 2 arguments expected, 1 provided"
        $ [] =>> (Function [string, string] string) $? [string]

cases : IO ()
cases = do canAssignTypeToItself
           cannotAssignTypeToADifferentPrimitive
           canAssignTypeToItsOwnAlias
           canAssignAliasToItsResolution
           invalidSourceAliasIsATypeError
           invalidTargetAliasIsATypeError
           canAssignFunctionToItself
           cannotAssignFunctionToOneReturningADifferentType
           cannotAssignFunctionToOneTakingADifferentType
           cannotAssignFunctionToOneOfDifferentArity
           cannotAssignFunctionToPrimitive
           callingAFunctionWithCorrectParamsYieldsReturnType
           cannotCallFunctionWithArgumentsOfDifferentTypes
           cannotCallFunctionWithWrongNumberOfArguments
