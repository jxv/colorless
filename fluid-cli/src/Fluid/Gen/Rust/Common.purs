module Fluid.Gen.Rust.Common where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_)
import Fluid.Gen.Spec (Version)
import Fluid.Gen.Lines
import Fluid.Gen.Plan

genWrap :: Wrap -> Lines Unit
genWrap {name, type: type', label, instances: {text, number}} = do
  line ""
  addLine ["// Wrap: ", name]
  line "#[derive(Debug)]"
  addLine ["struct ", name, "(", type',");"]

genStruct :: Struct -> Lines Unit
genStruct {name, label, members} = do
  line ""
  addLine ["// Struct: ", name]
  line "#[derive(Debug)]"
  addLine ["struct ", name, " {"]
  flip traverse_ members $ \member ->
    addLine ["    ", member.name, ": ", member.type, ","]
  line "}"

genEnumeration :: Enumeration -> Lines Unit
genEnumeration {name, enumerals} = do
  line ""
  addLine ["// Enumeration: ", name]
  line "#[derive(Debug)]"
  addLine ["enum ", name, " {"]

  flip traverse_ enumerals $ \enumeral ->
    addLine $
      ["    ", enumeral.tag] <>
      (case enumeral.members of
        Nothing -> [""]
        Just members ->
          [" { "] <>
          (flip Array.concatMap members $ \member -> [member.name, ": ", member.type, ","]) <>
          [" }"]
          ) <>
      [","]
