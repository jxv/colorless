module Fluid.Gen.Crystal.Common where

import Fluid.Gen.Lines
import Fluid.Gen.Plan
import Prelude

import Data.Traversable (intercalate, traverse_)

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["# Struct: ", name]
  addLine ["struct ", name]
  addLine $
    ["  property "] <>
    [intercalate ", " (map (\{name} -> name) members)]
  addLine $
    ["  def initialize("] <>
    [intercalate ", " (map (\m -> "@" <> m.name <> " : " <> m.type) members)] <>
    [")"]
  line "  end"
  line "end"
