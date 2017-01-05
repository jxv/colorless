module Test.Colorless.Parser.TokenSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Parser.Atomic
import Colorless.Parser.Token
import Colorless.Parser.Prototypes

import Control.Applicative (Alternative(..))

mkFixture "Fixture" [ts|Atomic|]

spec :: Spec
spec = do
  describe "module override" $ do
    it "initiateModuleOverride' happy path" $ do
      functions <- functionsT def
        { _match = \a -> lift $ a `shouldBe` "-"
        }
        initiateModuleOverride'
      functions `shouldBe` ["match"]

  describe "tokens" $ do
    it "moduleReferenceToken'" $ do
      (value, functions) <- valueFunctionsT def
        { _match = \a -> lift $ a `shouldBe` "%"
        , _upperCamelCase = return "MyModule"
        }
        moduleReferenceToken'
      functions `shouldBe` ["match","upperCamelCase"]
      value `shouldBe` (ModuleReference "MyModule")
