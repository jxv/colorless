module Fluid.Gen.PureScript.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "()"
  , bool: "P.Boolean"
  , int: "P.Int"
  , float: "P.Number"
  , char: "P.Char"
  , string: "P.String"
  , list: \x -> "[" <> x <> "]"
  , option: \x -> "(P.Maybe " <> x <> ")"
  , either: \x y -> "(P.Either (" <> x <> ") (" <> y <> "))"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , struct: \s -> s
  , member: \m -> m
  }
