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

  describe "Multiple equivalent names for the same type" $ do
    it "With two indexes, either can access the other's bindings" $ do
      let retrievedType = do
        bindings <- pure [] >>= equivalent 1 3 >>= bindType 1 MSuccess
        lookupMType bindings 3
      retrievedType \@/ MSuccess
    it "With multiple indexes, can access the other's bindings transitively" $ do
      let retrievedType = do
        bindings <- pure [] >>= equivalent 1 3 >>= equivalent 3 5 >>= bindType 1 MSuccess
        lookupMType bindings 5
      retrievedType \@/ MSuccess
    it "Introducing an equivalence with one type already bound binds the other type too" $ do
      let retrievedType = do
        bindings <- pure [] >>= bindType 1 MSuccess >>= equivalent 1 3
        lookupMType bindings 3
      retrievedType \@/ MSuccess
    it "Introducing an equivalence between two bound types which don't match is a type error" $ do
      let retrievedType = do
        bindings <- pure [] >>= bindType 1 MSuccess >>= bindType 3 MString >>= equivalent 1 3
        lookupMType bindings 3
      retrievedType >.< MkTypeError "Type mismatch: cannot unify Success and String"
