module Test.Colorless.LexerSpec (spec) where

import Pregame
import Data.Either.Combinators (rightToMaybe)
import Test.Hspec
import Test.Fixie
import Colorless.Parser.Types
import Colorless.Parser.Lexer
import qualified Text.Megaparsec as P

spec :: Spec
spec = do
  describe "lex" $ do
    it "parse lowerCaseCamel" $ do
      mLex <- rightToMaybe <$> runLexerM lex "str"
      mLex `shouldBe` Just (LexLowerCamelCase "str")
    it "parse colon" $ do
      mLex <- rightToMaybe <$> runLexerM lex ":"
      mLex `shouldBe` Just LexColon
  describe "lexer" $ do
    it "simple function" $ do
      let reverseLex :: [LexToken]
          reverseLex =
              [ LexToken (Loc 1 1) (LexLowerCamelCase "reverse")
              , LexToken (Loc 1 9) (LexLowerCamelCase "str")
              , LexToken (Loc 1 12) LexColon
              , LexToken (Loc 1 14) (LexLowerCamelCase "str")
              ]
      result <- lexer "reverse str: str"
      result `shouldBe` Just reverseLex
