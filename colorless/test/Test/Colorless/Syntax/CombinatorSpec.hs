module Test.Colorless.Syntax.CombinatorSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Syntax.Atomic
import Colorless.Syntax.Combinator
import Colorless.Syntax.Types
import Colorless.Syntax.Monad

spec :: Spec
spec = do
  describe "choice'" $
    it "should parse \"ghi\" as \"ghi\" from a list of other potential parsers" $ do
      let alphabet = [literal "abc", literal "def", literal "ghi"]
      actual <- liftIO $ runSyntaxM (choice' alphabet) "ghi"
      actual `shouldBe` Right "ghi"
