module Fluid.Gen.Go.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "bool"
  , int: "int64"
  , float: "float64"
  , char: "char"
  , string: "string"
  , list: \_x -> "List"
  , option: \x -> x <> "*"
  , either: \_x _y -> "Either"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
