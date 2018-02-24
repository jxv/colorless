module Fluid.Gen.Conversion where

import Prelude ((/=), (==), (<>), (&&), map)
import Data.String (toUpper) as String
import Data.String (uncons, joinWith)
import Data.String (toCharArray, fromCharArray)
import Data.Char (toLower, toUpper)
import Data.Maybe (Maybe(..))

type Conversion =
  { unit :: String
  , bool :: String
  , int :: String
  , float :: String
  , char :: String
  , string :: String
  , list :: String -> String
  , option :: String -> String
  , either :: String -> String -> String
  , label :: String -> String
  , version :: Int -> String -> String
  , ty :: String -> String
  , member :: String -> String
  , tag :: String -> String
  }

snakecase :: String -> String
snakecase s = case uncons s of
  Nothing -> ""
  Just {head,tail} -> fromCharArray [toLower head] <> joinWith "" (map prependUnderscore (toCharArray tail))
  where
    prependUnderscore c = if isUpper c then fromCharArray ['_',toLower c] else fromCharArray [c]

screamingSnakecase :: String -> String
screamingSnakecase s = String.toUpper (snakecase s)

isUpper :: Char -> Boolean
isUpper c = c /= toLower c && c == toUpper c

lowerFirst :: String -> String
lowerFirst s = case uncons s of
  Nothing -> ""
  Just {head,tail} -> fromCharArray [toLower head] <> tail
