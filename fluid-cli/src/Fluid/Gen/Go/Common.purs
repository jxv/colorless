module Fluid.Gen.Go.Common where

import Prelude
import Data.Traversable (traverse_)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine ["type ", name, " struct {"]
  flip traverse_ members $ \member ->
    addLine ["    ", member.name, " ", member.type]
  line "}"
