module Fluid.Gen.TypeScript.ServerVersion where

import Prelude (discard, (==), show)
import Data.Array as Array
import Data.Traversable (traverse_)

import Fluid.Gen.Plan (Enumeral, Enumeration, Func, Plan, Struct, Wrap, lowercaseFirstLetter, uppercaseFirstLetter, PullPlan)
import Fluid.Gen.Spec (Version, filterVersion)
import Fluid.Gen.Lines (linesContent, line, addLine)

import Fluid.Gen.TypeScript.Common

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do
  let currentStructs = filterVersion plan.version plan.structs
  line ""
  line "////////////////////////////////////////////////////////"
  line "// Types"
  line "////////////////////////////////////////////////////////"
  traverse_ genStruct currentStructs
