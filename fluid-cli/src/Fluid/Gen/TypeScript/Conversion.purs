module Fluid.Gen.TypeScript.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "boolean"
  , int: "number"
  , float: "number"
  , char: "string"
  , string: "string"
  , list: \x -> "Array<" <> x <> ">"
  , option: \x -> "(" <> x <> " | null)"
  , either: \x y -> "(" <> x <> " | " <> y <> ")"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "Major" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  }
