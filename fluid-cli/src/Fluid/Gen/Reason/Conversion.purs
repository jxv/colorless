module Fluid.Gen.Reason.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "bool"
  , int: "int"
  , float: "float"
  , char: "string"
  , string: "string"
  , list: \x -> "list(" <> x <> ")"
  , option: \x -> "option(" <> x <> ")"
  , either: \x y -> "either(" <> x <> "," <> y <> ")"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  , tag: \t -> t
  }
