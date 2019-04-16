module BindingsTests

import Specdris.Spec

import Minima.Annotators.MTypes
import Minima.Annotators.UniqueIndexer
import Minima.Annotators.Bindings
import Test.Support.EitherResults

%access public export

specs : IO ()
specs = spec $ do
  describe "Binding types, not thinking about equivalences" $ do
    it "Adds a binding to an empty binding" $ do
      let retrievedType = do
        bindings <- bindType 1 MSuccess []
        lookupMType bindings 1
      retrievedType \@/ MSuccess
    it "Retrieves a type when multiple types have been bound" $ do
      let retrievedType = do
        bindings <- pure [] >>= bindType 1 MSuccess >>= bindType 2 MString
        lookupMType bindings 2
      retrievedType \@/ MString
    it "Reports an error when failing to find a type" $ do
      let retrievedType = do
        bindings <- bindType 1 MSuccess []
        lookupMType bindings 42
      retrievedType >.< MkTypeError "Index 42 not bound"
    it "Reports an error when incompatible types bound to the same index" $ do
      let retrievedType = do
        bindings <- pure [] >>= bindType 1 MSuccess >>= bindType 1 MString
        lookupMType bindings 1
      retrievedType >.< MkTypeError "Type mismatch for index 1: both String and Success bound."
    it "Allows the same type to be bound to the same index multiple times" $ do
      let retrievedType = do
        bindings <- pure [] >>= bindType 1 MSuccess >>= bindType 1 MSuccess
        lookupMType bindings 1
      retrievedType \@/ MSuccess
