module Fluid.Gen.Nim.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "bool"
  , int: "int32"
  , float: "float64"
  , char: "char"
  , string: "string"
  , list: \x -> "seq[" <> x <> "]"
  , option: \x -> "Option*[" <> x <> "]"
  , either: \x y -> "Either*[" <> x <> "," <> y <> "]"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "major" <> show major <> "::" <> x
  , ty: \s -> s
  , member: \m -> m
  }
