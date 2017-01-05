module Test.Colorless.Parser.AtomicSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie
import Text.Megaparsec.Prim

import Colorless.Parser.Atomic
import Colorless.Parser.Prototypes
import Colorless.Parser

spec :: Spec
spec = do
  describe "token'" $
    it "should parse \"()\" as ()" $ do
    actual <- liftIO $ runParserM (token' "()" ()) "()"
    actual `shouldBe` Right ()
  describe "match'" $
    it "should parse \"abc\"" $ do
      actual <- liftIO $ runParserM (match' "abc") "abc"
      actual `shouldBe` Right ()
  describe "literal'" $
    it "should parse \"abc\"" $ do
      actual <- liftIO $ runParserM (literal' "abc") "abc"
      actual `shouldBe` Right "abc"
  describe "eol'" $ do
    it "should parse \"\\n\"" $ do
      actual <- liftIO $ runParserM eol' "\n"
      actual `shouldBe` Right ()
    it "should parse \"\\r\\n\"" $ do
      actual <- liftIO $ runParserM eol' "\r\n"
      actual `shouldBe` Right ()
  describe "space'" $
    it "should parse \" \"" $ do
      actual <- liftIO $ runParserM space' " "
      actual `shouldBe` Right ()
  describe "char'" $
    it "should parse \'c\'" $ do
      actual <- liftIO $ runParserM (char' 'c') "c"
      actual `shouldBe` Right ()
  describe "satisfy'" $ do
    let test ch = ch `elem` (['a'..'f'] ++ ['0'..'9'])
    context "should parse any characters between a-f and 0-9" $ do
      it "a" $ do
        actual <- liftIO $ runParserM (satisfy' test) "a"
        actual `shouldBe` Right 'a'
      it "9" $ do
        actual <- liftIO $ runParserM (satisfy' test) "9"
        actual `shouldBe` Right '9'
  describe "integer'" $ do
    it "should parse \"432423\"" $ do
      actual <- liftIO $ runParserM integer' "432423"
      actual `shouldBe` Right 432423
    it "should parse \"-34\"" $ do
      actual <- liftIO $ runParserM integer' "-34"
      actual `shouldBe` Right (-34)
  describe "lowerCamelCase'" $
    it "should parse \"abcDefGhiJKLMnOp1234\"" $ do
      actual <- liftIO $ runParserM lowerCamelCase' "abcDefGhiJKLMnOp1234"
      actual `shouldBe` Right "abcDefGhiJKLMnOp1234"
  describe "upperCamelCase'" $
    it "should parse \"AbcDefGhiJKLMnOp1234\"" $ do
      actual <- liftIO $ runParserM upperCamelCase' "AbcDefGhiJKLMnOp1234"
      actual `shouldBe` Right "AbcDefGhiJKLMnOp1234"
  describe "parens'" $ do
    it "parse a \"(c)\" as \"c\"" $ do
      actual <- liftIO $ runParserM (parens' (literal "c")) "(c)"
      actual `shouldBe` Right "c"
    it "parse a \"(((abc)))\" as \"abc\"" $ do
      actual <- liftIO $ runParserM (parens' $ parens' $ parens' (literal "abc")) "(((abc)))"
      actual `shouldBe` Right "abc"
