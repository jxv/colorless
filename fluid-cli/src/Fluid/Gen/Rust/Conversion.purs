module Fluid.Gen.Rust.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "()"
  , bool: "bool"
  , int: "i32"
  , float: "f64"
  , char: "char"
  , string: "str"
  , list: \x -> "std::collections::LinkedList<" <> x <> ">"
  , option: \x -> "std::Option<" <> x <> ">"
  , either: \x y -> "std::Result<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "major" <> show major <> "::" <> x
  }
