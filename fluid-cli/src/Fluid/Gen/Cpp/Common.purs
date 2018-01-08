module Fluid.Gen.Cpp.Common where

import Prelude
import Data.Traversable (traverse_)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

declareStruct :: Struct -> Lines Unit
declareStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine ["struct ", name, ";"]

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine ["struct ", name, " {"]
  flip traverse_ members $ \member ->
    addLine ["   ", member.type, " ", member.name, ";"]
  line "};"
