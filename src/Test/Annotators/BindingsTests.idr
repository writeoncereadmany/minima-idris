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
        bindings <- bindType 1 MSuccess []
        bindings' <- bindType 2 MString bindings
        lookupMType bindings' 2
      retrievedType \@/ MString
