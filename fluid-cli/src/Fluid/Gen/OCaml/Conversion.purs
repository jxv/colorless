module Fluid.Gen.OCaml.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "unit"
  , bool: "bool"
  , int: "int"
  , float: "double"
  , char: "char"
  , string: "string"
  , list: \x -> "(" <> x <> " list)"
  , option: \x -> x <> " maybe"
  , either: \x y -> "((" <> x <> "," <> y <> ") result)"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , struct: \s -> s
  , member: \m -> m
  }
