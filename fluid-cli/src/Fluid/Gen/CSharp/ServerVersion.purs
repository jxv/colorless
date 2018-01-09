module Fluid.Gen.CSharp.ServerVersion where

import Prelude
import Data.Traversable

import Fluid.Gen.Plan (Plan)
import Fluid.Gen.Spec (Version, filterVersion)
import Fluid.Gen.Lines
import Fluid.Gen.CSharp.Common

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do
  let currentStructs = filterVersion plan.version plan.structs

  line ""
  line "////////////////////////////////////////////////////////"
  line "// Types"
  line "////////////////////////////////////////////////////////"
  traverse_ genStruct currentStructs
  line ""
