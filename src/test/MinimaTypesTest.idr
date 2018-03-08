module MinimaTypesTest

import MinimaTypes
import test.Assertions

%access export

typechecks : Either TypeError b -> IO ()
typechecks = assertIsRight

yieldsTypeError : (Show b) => TypeError -> Either TypeError b -> IO ()
yieldsTypeError = assertLeft

yieldsType : MinimaType -> Either TypeError MinimaType -> IO ()
yieldsType = assertRight

yieldsContext : Context -> Either TypeError Context -> IO ()
yieldsContext = assertRight

bindsParameter : String -> MinimaType -> Either TypeError Context -> IO ()
bindsParameter param type result = assertRight (record { parametrics = [(param, type)]} emptyContext) result

string : MinimaType
string = Primitive "String"

number : MinimaType
number = Primitive "Number"

text : MinimaType
text = Alias "Text"

a : MinimaType
a = Parameter "a"

textAlias : (String, MinimaType)
textAlias = ("Text", string)



canAssignTypeToItself : IO ()
canAssignTypeToItself =
        typechecks
        $ emptyContext =>> string ->? string

cannotAssignTypeToADifferentPrimitive : IO ()
cannotAssignTypeToADifferentPrimitive =
        yieldsTypeError "Cannot assign String to Number"
        $ emptyContext =>> string ->? number

canAssignTypeToItsOwnAlias : IO ()
canAssignTypeToItsOwnAlias =
        typechecks
        $ record { aliases = [textAlias] } emptyContext =>> string ->? text

canAssignAliasToItsResolution : IO ()
canAssignAliasToItsResolution =
        typechecks
        $ record { aliases = [textAlias] } emptyContext =>> text ->? string

invalidSourceAliasIsATypeError : IO ()
invalidSourceAliasIsATypeError =
        yieldsTypeError "No such type Text found"
        $ emptyContext =>> text ->? string

invalidTargetAliasIsATypeError : IO ()
invalidTargetAliasIsATypeError =
        yieldsTypeError "No such type Text found"
        $ emptyContext =>> string ->? text

assigningConcreteTypeToUnboundIdentifierBindsIt : IO ()
assigningConcreteTypeToUnboundIdentifierBindsIt =
        bindsParameter "a" string
        $ emptyContext =>> string ->? a

canAssignFunctionToItself : IO ()
canAssignFunctionToItself =
        typechecks
        $ emptyContext =>> (Function [] string) ->? (Function [] string)

cannotAssignFunctionToOneReturningADifferentType : IO ()
cannotAssignFunctionToOneReturningADifferentType =
        yieldsTypeError "Cannot assign String to Number"
        $ emptyContext =>> (Function [] string) ->? (Function [] number)

cannotAssignFunctionToOneTakingADifferentType : IO ()
cannotAssignFunctionToOneTakingADifferentType =
        yieldsTypeError "Cannot assign Number to String"
        $ emptyContext =>> (Function [string] string) ->? (Function [number] string)

cannotAssignFunctionToOneOfDifferentArity : IO ()
cannotAssignFunctionToOneOfDifferentArity =
        yieldsTypeError "Arity mismatch"
        $ emptyContext =>> (Function [string] string) ->? (Function [] string)

cannotAssignFunctionToPrimitive : IO ()
cannotAssignFunctionToPrimitive =
        yieldsTypeError "Cannot assign [] => String to String"
        $ emptyContext =>> (Function [] string) ->? string

callingAFunctionWithCorrectParamsYieldsReturnType : IO ()
callingAFunctionWithCorrectParamsYieldsReturnType =
        yieldsType string
        $ emptyContext =>> (Function [] string) $? []

callingAParametricFunctionWillPopulateReturnType : IO ()
callingAParametricFunctionWillPopulateReturnType =
        yieldsType string
        $ emptyContext =>> (Function [a] a) $? [string]

callingAParametricFunctionWillPopulateReturnType : IO ()
callingAParametricFunctionWillPopulateReturnType =
        yieldsType string
        $ emptyContext =>> (Function [a] a) $? [string]

cannotCallFunctionWithArgumentsOfDifferentTypes : IO ()
cannotCallFunctionWithArgumentsOfDifferentTypes =
        yieldsTypeError "Cannot assign Number to String"
        $ emptyContext =>> (Function [string] string) $? [number]

cannotCallFunctionWithWrongNumberOfArguments : IO ()
cannotCallFunctionWithWrongNumberOfArguments =
        yieldsTypeError "Arity mismatch: 2 arguments expected, 1 provided"
        $ emptyContext =>> (Function [string, string] string) $? [string]

cannotCallNonFunction : IO ()
cannotCallNonFunction =
        yieldsTypeError "Type String is not a function"
        $ emptyContext =>> string $? [string]

cases : IO ()
cases = do canAssignTypeToItself
           cannotAssignTypeToADifferentPrimitive
           canAssignTypeToItsOwnAlias
           canAssignAliasToItsResolution
           invalidSourceAliasIsATypeError
           invalidTargetAliasIsATypeError
           assigningConcreteTypeToUnboundIdentifierBindsIt
           canAssignFunctionToItself
           cannotAssignFunctionToOneReturningADifferentType
           cannotAssignFunctionToOneTakingADifferentType
           cannotAssignFunctionToOneOfDifferentArity
           cannotAssignFunctionToPrimitive
           callingAFunctionWithCorrectParamsYieldsReturnType
           cannotCallFunctionWithArgumentsOfDifferentTypes
           cannotCallFunctionWithWrongNumberOfArguments
           cannotCallNonFunction
           callingAParametricFunctionWillPopulateReturnType
