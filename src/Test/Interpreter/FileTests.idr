module FileTests

import Test.Interpreter.MinimaPrelude
import Minima.Interpreter.Interpreter
import Test.Support.MockInteraction
import Specdris.SpecIO
import Lens

showErrors : Show a => String -> Either a b -> Either String b
showErrors filename (Left a) = Left (show a ++ ": " ++ filename)
showErrors _ (Right b) = Right b

runFile : String -> IO (Either String (List String))
runFile fileName = do source <- readFile ("examples/" ++ fileName)
                      pure $ showErrors fileName source >>= outputFrom

loadOutput : String -> IO (Either String (List String))
loadOutput outputFile = do result <- readFile ("examples/" ++ outputFile)
                           pure $ lines <$> showErrors outputFile result

infixl 2 ->\@/->
(->\@/->) : String -> String -> IO SpecResult
(->\@/->) fileName outputFile = do result <- runFile fileName
                                   expected <- loadOutput outputFile
                                   pure $ result === expected

export
specs : IO ()
specs = specIO $ do
  describe "On files" $ do
    it "Can run and assert output" $ do
      "helloworld.mma" ->\@/-> "helloworld.output"
