module Test.Colorless.SyntaxSpec (spec) where

import Pregame
import Data.Either.Combinators (rightToMaybe)
import Test.Hspec
import Test.Fixie
import Colorless.Parser.Types
import Colorless.Parser.Syntax
import qualified Text.Megaparsec as P

spec :: Spec
spec = do
  describe "fnName" $ do
    it "parse lower case camel as FnName" $ do
      result <- rightToMaybe <$> runSyntaxM fnName [LexToken (Loc 0 0) (LexLowerCamelCase "reverse")]
      result `shouldBe` Just "reverse"
