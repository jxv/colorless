module Fluid.Gen.Java.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "Boolean"
  , int: "Integer"
  , float: "Double"
  , char: "Char"
  , string: "String"
  , list: \x -> "ArrayList<" <> x <> ">"
  , option: \x -> "Optional<" <> x <> ">"
  , either: \x y -> "Either<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
