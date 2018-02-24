module Fluid.Gen.D.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "bool"
  , int: "int"
  , float: "double"
  , char: "char"
  , string: "std.string.String"
  , list: \x -> "std.container.dlist.DList!(" <> x <> ")"
  , option: \x -> "std.typecons.Nullable!(" <> x <> ")"
  , either: \x y -> "std.variant.Algebraic!(" <> x <> "," <> y <> ")"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  , ty: \s -> s
  , member: \m -> m
  , tag: \t -> t
  }
