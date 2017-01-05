module Test.Colorless.Parser.DeclarationSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie

import Colorless.Parser.Prototypes
import Colorless.Parser.Declaration
import Colorless.Parser.Token

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
