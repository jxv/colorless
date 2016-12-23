module Test.Colorless.ParserSpec (spec) where

import Pregame

import Test.Hspec
import Test.Fixie

import Colorless

mkFixture "Fixture" [ts|Console|]

spec :: Spec
spec = do
  describe "main" $ do
    it "should print the banner" $ do
      let fixture = def
            { _writeLine = \msg -> do
                lift $ msg `shouldBe` bannerMessage
                note "print banner"
            }
      calls <- notesT fixture main
      calls `shouldBe` ["print banner"]
