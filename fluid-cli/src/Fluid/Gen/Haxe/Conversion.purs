module Fluid.Gen.Haxe.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "Void"
  , bool: "Bool"
  , int: "Int"
  , float: "Float"
  , char: "String"
  , string: "String"
  , list: \x -> "Array<" <> x <> ">"
  , option: \x -> "Null<" <> x <> ">"
  , either: \x y -> "Either<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  }
