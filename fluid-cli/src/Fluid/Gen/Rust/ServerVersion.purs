module Fluid.Gen.Rust.ServerVersion where

import Prelude (discard, (==))
import Data.Array as Array
import Data.Traversable (traverse_)

import Fluid.Gen.Plan (Enumeral, Enumeration, Func, Plan, Struct, Wrap, lowercaseFirstLetter, uppercaseFirstLetter, PullPlan)
import Fluid.Gen.Spec (Version)
import Fluid.Gen.Lines (linesContent, line)

import Fluid.Gen.Rust.Common

filterVersion :: forall a. Version -> Array { major :: Int | a } -> Array { major :: Int | a }
filterVersion version = Array.filter (\ty -> ty.major == version.major)

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do

  let currentWraps = filterVersion plan.version plan.wraps
  let currentStructs = filterVersion plan.version plan.structs
  let currentEnumerations = filterVersion plan.version plan.enumerations

  line ""
  line "////////////////////////////////////////////////////////"
  line "// Types"
  line "////////////////////////////////////////////////////////"
  line ""
  traverse_ genWrap currentWraps
  traverse_ genStruct currentStructs
  traverse_ genEnumeration currentEnumerations
