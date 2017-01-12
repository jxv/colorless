module Test.Colorless.Syntax.DeclarationSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie

import Colorless.Syntax.Types
import Colorless.Syntax.Declaration
import Colorless.Syntax.Token

mkFixture "Fixture" [ts|Token|]

spec :: Spec
spec = do
  describe "declarations" $ do
    it "moduleOverrideDeclaration' happy path" $ do
      (functions, value) <- functionsValueT def
        { _initiateModuleOverride = return ()
        , _moduleReferenceToken = return "MyModule"
        , _moduleVersionToken = return 10
        , _moduleVersionSeparator = return ()
        }
        moduleOverrideDeclaration'
      functions `shouldBe` ["initiateModuleOverride","moduleReferenceToken","moduleVersionSeparator","moduleVersionToken"]
      value `shouldBe` ModuleOverrideDeclaration "MyModule" 10
