module Fluid.Gen.Swift.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "Void"
  , bool: "Bool"
  , int: "Int"
  , float: "Double"
  , char: "Char"
  , string: "String"
  , list: \x -> "[" <> x <> "]"
  , option: \x -> x <> "?"
  , either: \x y -> "Either<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , struct: \s -> s
  , member: \m -> m
  }
