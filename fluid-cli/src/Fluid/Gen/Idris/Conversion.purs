module Fluid.Gen.Idris.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "()"
  , bool: "Bool"
  , int: "Int"
  , float: "Double"
  , char: "Char"
  , string: "String"
  , list: \x -> "(List " <> x <> ")"
  , option: \x -> "(Maybe " <> x <> ")"
  , either: \x y -> "(Either " <> x <> " " <> y <> ")"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  , tag: \t -> t
  }
