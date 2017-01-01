module Test.Colorless.Parser.CombinatorSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Parser.Atomic
import Colorless.Parser.Combinator
import Colorless.Parser.Types
import Colorless.Parser

spec :: Spec
spec = do
  describe "choice'" $ do
    it "should parse \"c\" as 'c'" $ do
      let alphabet = literal "abc" :| [literal "def", literal "ghi"]
      actual <- liftIO $ runParserM (choice' alphabet) "ghi"
      actual `shouldBe` Right "ghi"
