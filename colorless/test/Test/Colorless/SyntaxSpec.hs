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
  describe "fnName" $
    it "parse lower case camel as FnName" $ do
      result <- rightToMaybe <$> runSyntaxM fnName [LexToken (Loc 0 0) (LexLowerCamelCase "reverse")]
      result `shouldBe` Just "reverse"
  describe "primTy" $ do
    it "unit" $ do
      result <- rightToMaybe <$> runSyntaxM primTy [LexToken (Loc 0 0) (LexLowerCamelCase "unit")]
      result `shouldBe` Just PrimTyUnit
    it "u8" $ do
      result <- rightToMaybe <$> runSyntaxM primTy [LexToken (Loc 0 0) (LexLowerCamelCase "u8")]
      result `shouldBe` Just PrimTyU8
    it "str" $ do
      result <- rightToMaybe <$> runSyntaxM primTy [LexToken (Loc 0 0) (LexLowerCamelCase "str")]
      result `shouldBe` Just PrimTyStr
  describe "fnDef" $
    it "parse" $ do
      result <- rightToMaybe <$> runSyntaxM fnDef
        [ LexToken (Loc 1 9) (LexLowerCamelCase "str")
        , LexToken (Loc 1 12) LexColon
        , LexToken (Loc 1 14) (LexLowerCamelCase "str")
        ]
      result `shouldBe` Just (FnDef [("str", MonoTyRefPrimTy PrimTyStr)] (MonoTyRefPrimTy PrimTyStr) mempty)
