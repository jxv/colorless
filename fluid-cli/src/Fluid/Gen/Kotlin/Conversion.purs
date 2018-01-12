module Fluid.Gen.Kotlin.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "Unit"
  , bool: "Boolean"
  , int: "Int"
  , float: "Double"
  , char: "Char"
  , string: "String"
  , list: \x -> "List<" <> x <> ">"
  , option: \x -> x <> "?"
  , either: \x y -> "Either<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
