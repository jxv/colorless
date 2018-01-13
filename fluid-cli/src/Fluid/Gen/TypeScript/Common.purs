module Fluid.Gen.TypeScript.Common where

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
  addLine ["// Struct: ", name]
  addLine ["interface ", name, " {"]
  flip traverse_ members $ \member ->
    addLine ["  ", member.name, ": ", member.type, ";"]
  line "}"
