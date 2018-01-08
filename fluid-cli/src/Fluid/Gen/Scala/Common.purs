module Fluid.Gen.Scala.Common where

import Prelude
import Data.Array as Array
import Data.Traversable (traverse_)
import Data.Foldable (intercalate)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine $
    ["case class ", name, "("] <>
    [intercalate ", " $ map (\m -> m.name <> ": " <> m.type) members] <>
    [")"]
