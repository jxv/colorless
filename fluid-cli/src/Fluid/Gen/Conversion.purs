module Fluid.Gen.Conversion where

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
  , struct :: String -> String
  , member :: String -> String
  }
