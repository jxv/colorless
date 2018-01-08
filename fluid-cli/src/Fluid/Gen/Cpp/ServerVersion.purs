module Fluid.Gen.Cpp.ServerVersion where

import Prelude
import Data.Traversable

import Fluid.Gen.Plan (Plan)
import Fluid.Gen.Spec (Version, filterVersion)
import Fluid.Gen.Lines
import Fluid.Gen.Cpp.Common

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do
  let currentStructs = filterVersion plan.version plan.structs

  line ""
  line "////////////////////////////////////////////////////////"
  line "// Types"
  line "////////////////////////////////////////////////////////"
  line ""
  traverse_ declareStruct currentStructs
  traverse_ genStruct currentStructs
  line ""
