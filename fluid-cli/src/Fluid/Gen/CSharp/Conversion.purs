module Fluid.Gen.CSharp.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "bool"
  , int: "int"
  , float: "double"
  , char: "char"
  , string: "string"
  , list: \x -> "ArrayList<" <> x <> ">"
  , option: \x -> "Option<" <> x <> ">"
  , either: \x y -> "Either<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , struct: \s -> s
  , member: \m -> m
  }
