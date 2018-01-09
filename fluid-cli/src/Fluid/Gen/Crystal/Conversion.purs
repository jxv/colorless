module Fluid.Gen.Crystal.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "Void"
  , bool: "Bool"
  , int: "Int64"
  , float: "Float64"
  , char: "Char"
  , string: "String"
  , list: \x -> "Array(" <> x <> ")"
  , option: \x -> "(" <> x <> " | Nil)"
  , either: \x y -> "(" <> x <> " | " <> y <> ")"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
