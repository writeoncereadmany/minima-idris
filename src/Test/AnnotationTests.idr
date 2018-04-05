module AnnotationTests

import Specdris.Spec
import Minima.AST
import Minima.Record
import Minima.Whatever
import Minima.Annotations
import Minima.Parsing.Parser
import Test.Support.EitherResults
import Lightyear.Strings
import Lightyear.Position
import Lightyear

%access export

Whatever String where
  whatever = ""

specs : IO ()
specs = spec $ do
  describe "Can strip annotations" $ do
    it "From parsed AST with position data" $ do
      let ast = parse expression "foo"
      (stripAnnotations <$> ast) \@/ Variable () "foo"
