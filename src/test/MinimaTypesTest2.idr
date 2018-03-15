module MinimaTypesTest2

import test.Assertions
import MinimaTypes2

%access export

string : DeBruijnIndex
string = (0, 0)

number : DeBruijnIndex
number = (0, 1)

text : DeBruijnIndex
text = (0, 2)

stringImpl : MinimaType2
stringImpl = Primitive string

numberImpl : MinimaType2
numberImpl = Primitive number

primitives : Bindings
primitives = [(string, stringImpl), (number, numberImpl)]

plus : DeBruijnIndex
plus = (0, 3)

plusImpl : MinimaType2
plusImpl = Function [number, number] number

prelude : Bindings
prelude = (plus, plusImpl) :: primitives

typechecks : Either TypeErrors _ -> IO ()
typechecks = assertIsRight

yieldsTypeErrors : Show b => List String -> Either TypeErrors b -> IO ()
yieldsTypeErrors = assertLeft

yieldsTypeError : Show b => String -> Either TypeErrors b -> IO ()
yieldsTypeError message = yieldsTypeErrors [ message ]

doesNotNeedBindingsToAssignTypeToItself : IO ()
doesNotNeedBindingsToAssignTypeToItself =
        typechecks
        $ [] |=> string ->? string

canAssignTypeToItsAlias : IO ()
canAssignTypeToItsAlias =
        typechecks
        $ [(string, stringImpl), (text, stringImpl)] |=> string ->? text

cannotAssignOnePrimitiveToAnother : IO ()
cannotAssignOnePrimitiveToAnother =
        yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ primitives |=> string ->? number

cannotAssignPrimitiveToFunction : IO ()
cannotAssignPrimitiveToFunction =
        yieldsTypeError "Cannot assign (0, 1) to [(0, 1), (0, 1)] => (0, 1)"
        $ prelude |=> number ->? plus

cannotAssignFunctionToPrimitive : IO ()
cannotAssignFunctionToPrimitive =
        yieldsTypeError "Cannot assign [(0, 1), (0, 1)] => (0, 1) to (0, 1)"
        $ prelude |=> plus ->? number

canAssignOneFunctionToItsAlias : IO ()
canAssignOneFunctionToItsAlias =
    let minus = (0, 4)
        newPrelude = (minus, plusImpl) :: prelude
     in typechecks
        $ newPrelude |=> plus ->? minus

cannotAssignFunctionWhereArgumentsDiffer : IO ()
cannotAssignFunctionWhereArgumentsDiffer =
    let charAt = (0, 5)
        charAtImpl = Function [number, string] number
        bindings = (charAt, charAtImpl) :: prelude
     -- type error here is invoking plus[] with args for charAt[]
     -- problem is in second argument: can't assign string (legal charAt arg)
     -- to number (the arg plus expects)
     in yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
        $ bindings |=> plus ->? charAt

cannotAssignFunctionWhereReturnTypesDiffer : IO ()
cannotAssignFunctionWhereReturnTypesDiffer =
    let stringConcat = (0, 5)
        stringConcatImpl = Function [number, number] string
        bindings = (stringConcat, stringConcatImpl) :: prelude
     -- type error here is invoking plus[] returns number (0, 1), whereas
     -- we're expecting a string (0, 0), and we can't assign a number to
     -- a string
     in yieldsTypeError "Cannot assign (0, 1) to (0, 0)"
        $ bindings |=> plus ->? stringConcat

canAssignParametricFunctionToConcreteOne : IO ()
canAssignParametricFunctionToConcreteOne =
     let a = (0, 6)
         genericFunction = (0, 7)
         genericFunctionImpl = Function [a, a] a
         newPrelude = (genericFunction, genericFunctionImpl) :: prelude
      in typechecks
         $ newPrelude |=> genericFunction ->? plus

cannotAssignConcreteFunctionToParametricOne : IO ()
cannotAssignConcreteFunctionToParametricOne =
    let a = (0, 6)
        genericFunction = (0, 7)
        genericFunctionImpl = Function [a, a] a
        bindings = (genericFunction, genericFunctionImpl) :: prelude
     in yieldsTypeErrors ["Source type (0, 6) must be bound", "Source type (0, 6) must be bound"]
        $ bindings |=> plus ->? genericFunction

cannotAssignParametricFunctionWhenBindingArgTypesMeansReturnTypesMismatch : IO ()
cannotAssignParametricFunctionWhenBindingArgTypesMeansReturnTypesMismatch =
     let a = (0, 6)
         genericFunction = (0, 7)
         genericImpl = Function [a] a
         lengthFunction = (0, 8)
         lengthImpl = Function [string] number
         bindings = (genericFunction, genericImpl) :: (lengthFunction, lengthImpl) :: prelude
      -- type error here is that assigning generic function to lenghtfunction binds
      -- a to string, which means generic function returns string, which then can't be
      -- assigned to a number for the return type
      in yieldsTypeError "Cannot assign (0, 0) to (0, 1)"
         $ bindings |=> genericFunction ->? lengthFunction


cases : IO ()
cases = do putStrLn "  ** Test suite MinimaTypesTest2: "
           doesNotNeedBindingsToAssignTypeToItself
           canAssignTypeToItsAlias
           cannotAssignOnePrimitiveToAnother
           cannotAssignPrimitiveToFunction
           cannotAssignFunctionToPrimitive
           canAssignOneFunctionToItsAlias
           cannotAssignFunctionWhereArgumentsDiffer
           canAssignParametricFunctionToConcreteOne
           cannotAssignConcreteFunctionToParametricOne
           cannotAssignParametricFunctionWhenBindingArgTypesMeansReturnTypesMismatch
           cannotAssignFunctionWhereReturnTypesDiffer
