module Test.ColorlessSpec (spec) where

import Pregame

import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Colorless

mkFixture "Fixture" [ts|Console|]

spec :: Spec
spec = do
  describe "main" $ do
    it "should print the banner" $ do
      calls <- logTestFixtureT main def
        { _writeLine = \msg -> do
            lift $ msg `shouldBe` bannerMessage
            log "print banner"
        }
      calls `shouldBe` ["print banner"]
