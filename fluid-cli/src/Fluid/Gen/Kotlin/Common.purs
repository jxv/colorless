module Fluid.Gen.Kotlin.Common where

import Prelude
import Data.Traversable (traverse_)
import Data.Foldable (intercalate)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine $
    ["data class ", name, "("] <>
    [intercalate ", " (map (\m -> "val " <> m.name <> ": " <> m.type) members)] <>
    [")"]
