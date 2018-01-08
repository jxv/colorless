module Fluid.Gen.Cpp.Conversion where

import Prelude ((<>), (==), show)

import Fluid.Gen.Conversion

conversion :: Conversion
conversion =
  { unit: "void"
  , bool: "bool"
  , int: "int64_t"
  , float: "double"
  , char: "char"
  , string: "std::string"
  , list: \x -> "std::vector<" <> x <> ">"
  , option: \x -> "std::optional<" <> x <> ">"
  , either: \x y -> "std::variant<" <> x <> "," <> y <> ">"
  , label: \x -> if x == "tag" then "_tag" else x
  , version: \major x -> "V" <> show major <> "." <> x
  }
