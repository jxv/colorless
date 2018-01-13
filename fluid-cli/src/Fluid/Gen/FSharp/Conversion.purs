module Fluid.Gen.FSharp.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "unit"
  , bool: "bool"
  , int: "int"
  , float: "float"
  , char: "char"
  , string: "string"
  , list: \x -> "List<" <> x <> ">"
  , option: \x -> "Option<" <> x <> ">"
  , either: \x y -> "Either<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  }
