module Fluid.Gen.Nim.Common where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_)
import Fluid.Gen.Spec (Version)
import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, label, members} = do
  line ""
  line "type"
  addLine ["# Struct: ", name]
  addLine ["  ", name, " = object"]
  flip traverse_ members $ \member ->
    addLine ["    ", member.name, ": ", member.type]
