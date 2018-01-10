module Fluid.Gen.FSharp.Common where

import Prelude
import Data.Traversable (traverse_)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine ["type ", name, " ="]
  lineList members
    "  { "
    "  , "
    (\member -> [member.name, " : ", member.type])
  line "  }"
