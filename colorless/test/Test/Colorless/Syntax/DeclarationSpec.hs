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
        , _moduleRefToken = return "MyModule"
        , _moduleVersionToken = return 10
        , _moduleVersionSeparator = return ()
        }
        moduleOverrideDecl'
      functions `shouldBe` ["initiateModuleOverride","moduleRefToken","moduleVersionSeparator","moduleVersionToken"]
      value `shouldBe` ModuleOverrideDecl "MyModule" 10
