module Test.Colorless.Syntax.AtomicSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Syntax.Atomic
import Colorless.Syntax.Types
import Colorless.Syntax.Monad

spec :: Spec
spec = do
  describe "token'" $
    it "should parse \"()\" as ()" $ do
    actual <- liftIO $ runSyntaxM (token' "()" ()) "()"
    actual `shouldBe` Right ()
  describe "match'" $
    it "should parse \"abc\"" $ do
      actual <- liftIO $ runSyntaxM (match' "abc") "abc"
      actual `shouldBe` Right ()
  describe "literal'" $
    it "should parse \"abc\"" $ do
      actual <- liftIO $ runSyntaxM (literal' "abc") "abc"
      actual `shouldBe` Right "abc"
  describe "eol'" $ do
    it "should parse \"\\n\"" $ do
      actual <- liftIO $ runSyntaxM eol' "\n"
      actual `shouldBe` Right ()
    it "should parse \"\\r\\n\"" $ do
      actual <- liftIO $ runSyntaxM eol' "\r\n"
      actual `shouldBe` Right ()
  describe "space'" $
    it "should parse \" \"" $ do
      actual <- liftIO $ runSyntaxM space' " "
      actual `shouldBe` Right ()
  describe "char'" $
    it "should parse \'c\'" $ do
      actual <- liftIO $ runSyntaxM (char' 'c') "c"
      actual `shouldBe` Right ()
  describe "satisfy'" $ do
    let test ch = ch `elem` (['a'..'f'] ++ ['0'..'9'])
    context "should parse any characters between a-f and 0-9" $ do
      it "a" $ do
        actual <- liftIO $ runSyntaxM (satisfy' test) "a"
        actual `shouldBe` Right 'a'
      it "9" $ do
        actual <- liftIO $ runSyntaxM (satisfy' test) "9"
        actual `shouldBe` Right '9'
  describe "integer'" $ do
    it "should parse \"432423\"" $ do
      actual <- liftIO $ runSyntaxM integer' "432423"
      actual `shouldBe` Right 432423
    it "should parse \"-34\"" $ do
      actual <- liftIO $ runSyntaxM integer' "-34"
      actual `shouldBe` Right (-34)
  describe "lowerCamelCase'" $
    it "should parse \"abcDefGhiJKLMnOp1234\"" $ do
      actual <- liftIO $ runSyntaxM lowerCamelCase' "abcDefGhiJKLMnOp1234"
      actual `shouldBe` Right "abcDefGhiJKLMnOp1234"
  describe "upperCamelCase'" $
    it "should parse \"AbcDefGhiJKLMnOp1234\"" $ do
      actual <- liftIO $ runSyntaxM upperCamelCase' "AbcDefGhiJKLMnOp1234"
      actual `shouldBe` Right "AbcDefGhiJKLMnOp1234"
  describe "parens'" $ do
    it "parse a \"(c)\" as \"c\"" $ do
      actual <- liftIO $ runSyntaxM (parens' (literal "c")) "(c)"
      actual `shouldBe` Right "c"
    it "parse a \"(((abc)))\" as \"abc\"" $ do
      actual <- liftIO $ runSyntaxM (parens' $ parens' $ parens' (literal "abc")) "(((abc)))"
      actual `shouldBe` Right "abc"
