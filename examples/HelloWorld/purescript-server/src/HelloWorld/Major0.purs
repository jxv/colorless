-- Module
module HelloWorld.Major0
  ( helloWorld'version
  , Hello(..)
  ) where

-- Imports
import Data.Maybe as Maybe
import Data.Either as Either

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
helloWorld'version :: { major :: Int, minor :: Int }
helloWorld'version = { major: 0, minor: 0 }

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Struct: Hello
data Hello = Hello
  { target :: String
  }

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

--------------------------------------------------------
-- Spec
--------------------------------------------------------