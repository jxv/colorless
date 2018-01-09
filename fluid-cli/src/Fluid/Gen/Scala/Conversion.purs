module Fluid.Gen.Scala.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "scala.Unit"
  , bool: "scala.Boolean"
  , int: "scala.Int"
  , float: "scala.Double"
  , char: "scala.Char"
  , string: "String"
  , list: \x -> "scala.Array[" <> x <> "]"
  , option: \x -> "scala.Option[" <> x <> "]"
  , either: \x y -> "scala.util.Either[" <> x <> "," <> y <> "]"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
