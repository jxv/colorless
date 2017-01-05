module Test.Colorless.Parser.CombinatorSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Parser.Atomic
import Colorless.Parser.Combinator
import Colorless.Parser.Prototypes
import Colorless.Parser

spec :: Spec
spec = do
  describe "choice'" $
    it "should parse \"ghi\" as \"ghi\" from a list of other potential parsers" $ do
      let alphabet = [literal "abc", literal "def", literal "ghi"]
      actual <- liftIO $ runParserM (choice' alphabet) "ghi"
      actual `shouldBe` Right "ghi"
