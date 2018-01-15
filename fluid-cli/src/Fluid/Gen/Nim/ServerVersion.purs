module Fluid.Gen.Nim.ServerVersion where

import Prelude (discard, (==), show)
import Data.Array as Array
import Data.Traversable (traverse_)

import Fluid.Gen.Plan (Enumeral, Enumeration, Func, Plan, Struct, Wrap, lowercaseFirstLetter, uppercaseFirstLetter, PullPlan)
import Fluid.Gen.Spec (Version, filterVersion)
import Fluid.Gen.Lines (linesContent, line, addLine)

import Fluid.Gen.Nim.Common

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do
  line "////////////////////////////////////////////////////////"
  line "// Types"
  line "////////////////////////////////////////////////////////"
  traverse_ genStruct plan.structs
  line ""
