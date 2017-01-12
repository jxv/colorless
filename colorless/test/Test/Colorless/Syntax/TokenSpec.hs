module Test.Colorless.Syntax.TokenSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Syntax.Atomic
import Colorless.Syntax.Token
import Colorless.Syntax.Types

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
        moduleRefToken'
      functions `shouldBe` ["match","upperCamelCase"]
      value `shouldBe` (ModuleRef "MyModule")
