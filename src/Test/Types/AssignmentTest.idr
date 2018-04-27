module AssignmentTest

import Minima.Types.MinimaTypes
import Minima.Types.Bindings
import Minima.Types.Assignment
import Minima.Types.TypeErrors
import Specdris.Spec

%access export

typechecksWhen : Show b => Either TypeErrors b -> SpecResult
typechecksWhen (Left errors) = UnaryFailure errors $ "should have typechecked"
typechecksWhen (Right _) = Success

yieldsTypeErrors : Show b => (actual : Either TypeErrors b) -> (expected : TypeErrors) -> SpecResult
yieldsTypeErrors (Left errors) expected = errors === expected
yieldsTypeErrors (Right bindings) expected = UnaryFailure bindings $ "should have yielded type error: " ++ show expected

yieldsTypeError : Show b => (actual : Either TypeErrors b) -> (expected : TypeError) -> SpecResult
yieldsTypeError actual expected = yieldsTypeErrors actual [expected]

yieldsBindings : Show a => (actual : Either a Bindings) -> (expected : Bindings) -> SpecResult
yieldsBindings (Left errors) expected = UnaryFailure errors $ "should have yielded bindings" ++ show expected
yieldsBindings (Right actual) expected = actual === expected

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

specs : IO ()
specs = spec $ do
  describe "Datatype assignments" $ do
    it "can assign data to itself" $ do
      typechecksWhen $
        [] |=> string ->? string
    it "cannot assign data to other data" $ do
      [] |=> string ->? number
        `yieldsTypeError` "Cannot assign (0, 0) to (0, 1)"
    it "can assign data to its alias" $ do
      let text = Named (1, 0)
      let bindings = [((1, 0), string)]
      typechecksWhen $
        bindings |=> string ->? text
    it "can assign alias to its root type" $ do
      let text = Named (1, 0)
      let bindings = [((1, 0), string)]
      typechecksWhen $
        bindings |=> text ->? string
    it "assigning to unbound binds it" $ do
      let param = Named (1, 0)
      [] |=> string ->? param
        `yieldsBindings` [((1, 0), string)]

  describe "Union assignments" $ do
    it "can assign member of union to union" $ do
      typechecksWhen $
        [] |=> string ->? maybeString
    it "cannot assign union to member of union" $ do
      [] |=> maybeString ->? string
        `yieldsTypeError` "Cannot assign (0, 2) to (0, 0)"
    it "can assign union to larger union" $ do
      typechecksWhen $
        [] |=> maybeString ->? maybeStringOrNumber
    it "cannot assign union to smaller union" $ do
      [] |=> maybeStringOrNumber ->? maybeString
        `yieldsTypeErrors` ["Cannot assign (0, 1) to (0, 0)", "Cannot assign (0, 1) to (0, 2)"]

  describe "Function assignments" $ do
    it "can assign function to itself" $ do
      let fun = Function [number, number] number
      typechecksWhen $
        [] |=> fun ->? fun
    it "cannot assign functions where arguments differ" $ do
      let id = Function [number] number
      let length = Function [string] number
      [] |=> id ->? length
        `yieldsTypeError` "Cannot assign (0, 0) to (0, 1)"
    it "cannot assign functions where return types differ" $ do
      let id = Function [number] number
      let toString = Function [number] string
      [] |=> id ->? toString
        `yieldsTypeError` "Cannot assign (0, 1) to (0, 0)"

  describe "Functions and subtyping" $ do
    it "can assign function where source arg supertype of target arg" $ do
      let id = Function [string] string
      let showMaybe = Function [maybeString] string
      typechecksWhen $
        [] |=> showMaybe ->? id
    it "cannot assign function where source arg subtype of target arg" $ do
      let id = Function [string] string
      let showMaybe = Function [maybeString] string
      [] |=> id ->? showMaybe
        `yieldsTypeError` "Cannot assign (0, 2) to (0, 0)"
    it "can assign function where source returns subtype of target return type" $ do
      let id = Function [string] string
      let verify = Function [string] maybeString
      typechecksWhen $
        [] |=> id ->? verify
    it "cannot assign function where source returns supertype of target return type" $ do
      let id = Function [string] string
      let verify = Function [string] maybeString
      [] |=> verify ->? id
        `yieldsTypeError` "Cannot assign (0, 2) to (0, 0)"

  describe "Parameteric functions" $ do
    it "can assign generic function to concrete function if unbound args line up" $ do
      let concrete = Function [string, number] number
      let parametric = Function [a, b] b
      typechecksWhen $
        [] |=> parametric ->? concrete
    it "cannot assign generic function to concrete function if unbound args to not line up" $ do
      let concrete = Function [string, number] number
      let parametric = Function [a, b] a
      [] |=> parametric ->? concrete
        `yieldsTypeError` "Cannot assign (0, 0) to (0, 1)"
    it "assigning args yields new bindings" $ do
      let param = Named (1, 0)
      assignArgs [] [param] [string]
        `yieldsBindings` [((1, 0), string)]
