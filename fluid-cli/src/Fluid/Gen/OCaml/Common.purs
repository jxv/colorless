module Fluid.Gen.OCaml.Common where

import Prelude
import Data.Traversable (traverse_)
import Data.Foldable (intercalate)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine ["type ", name, " = {"]
  flip traverse_ members $ \m ->
    addLine ["  ", m.name, " : ", m.type, ";"]
  line "}"
