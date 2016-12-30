module Test.Colorless.Parser.DeclarationSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie

import Colorless.Parser.Types
import Colorless.Parser.Declaration
import Colorless.Parser.Token

mkFixture "Fixture" [ts|Token|]

spec :: Spec
spec = do
  describe "declarations" $ do
    it "moduleOverrideDeclaration' happy path" $ do
      (functions, value) <- functionsValueT def
        { _initiateModuleOverride = return ()
        , _moduleReference = return "MyModule"
        , _moduleVersion = return 10
        }
        moduleOverrideDeclaration'
      functions `shouldBe` ["initiateModuleOverride","moduleReference","moduleVersion"]
      value `shouldBe` ModuleOverrideDeclaration "MyModule" 10
