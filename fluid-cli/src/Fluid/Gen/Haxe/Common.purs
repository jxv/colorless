module Fluid.Gen.Haxe.Common where

import Prelude
import Data.Traversable (traverse_)
import Data.Foldable (intercalate)

import Fluid.Gen.Lines
import Fluid.Gen.Plan

genStruct :: Struct -> Lines Unit
genStruct {name, members, indirection} = do
  line ""
  addLine ["// Struct: ", name]
  addLine ["class ", name, " {"]
  flip traverse_ members $ \member ->
    addLine ["    public var ", member.name, ":", member.type, ";"]
  line ""
  addLine $
    ["    public inline function new("] <>
    [intercalate ", " (map (\m -> m.name <> ":" <> m.type) members)] <>
    [") {"]
  flip traverse_ members $ \member ->
    addLine ["        this.", member.name, " = ", member.name, ";"]
  line "    }"
  line "}"
