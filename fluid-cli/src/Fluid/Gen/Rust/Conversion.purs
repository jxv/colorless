module Fluid.Gen.Rust.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "()"
  , bool: "P.Bool"
  , int: "P.Int"
  , float: "P.Double"
  , char: "P.Char"
  , string: "R.Text"
  , list: \x -> "[" <> x <> "]"
  , option: \x -> "(P.Maybe " <> x <> ")"
  , either: \x y -> "(P.Either (" <> x <> ") (" <> y <> "))"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
