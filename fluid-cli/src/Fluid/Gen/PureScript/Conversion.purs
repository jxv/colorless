module Fluid.Gen.PureScript.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "()"
  , bool: "Boolean"
  , int: "Int"
  , float: "Number"
  , char: "Char"
  , string: "String"
  , list: \x -> "[" <> x <> "]"
  , option: \x -> "(Maybe.Maybe " <> x <> ")"
  , either: \x y -> "(Either.Either (" <> x <> ") (" <> y <> "))"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  }
