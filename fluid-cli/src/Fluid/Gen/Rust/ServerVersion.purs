module Fluid.Gen.Rust.ServerVersion where

import Prelude (discard, (==), show)
import Data.Array as Array
import Data.Traversable (traverse_)

import Fluid.Gen.Plan (Enumeral, Enumeration, Func, Plan, Struct, Wrap, lowercaseFirstLetter, uppercaseFirstLetter, PullPlan)
import Fluid.Gen.Spec (Version, filterVersion)
import Fluid.Gen.Lines (linesContent, line, addLine)

import Fluid.Gen.Rust.Common

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do

  let currentWraps = filterVersion plan.version plan.wraps
  let currentStructs = filterVersion plan.version plan.structs
  let currentEnumerations = filterVersion plan.version plan.enumerations

  addLine ["mod ", plan.prefix, "::major_", show plan.version.major," {"]
  line ""
  line "////////////////////////////////////////////////////////"
  line "// Types"
  line "////////////////////////////////////////////////////////"
  traverse_ genWrap currentWraps
  traverse_ genStruct currentStructs
  traverse_ genEnumeration currentEnumerations
  line "}"
