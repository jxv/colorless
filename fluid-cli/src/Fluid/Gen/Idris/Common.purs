module Fluid.Gen.Idris.Common where

import Prelude
import Data.Traversable (traverse_)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["-- Struct: ", name]
  addLine ["record ", name, " where"]
  addLine ["    constructor Mk", name]
  flip traverse_ members $ \member ->
    addLine ["    ", member.name, " : ", member.type]
