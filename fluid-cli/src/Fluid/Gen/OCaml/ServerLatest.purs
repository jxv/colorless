module Fluid.Gen.OCaml.ServerLatest where

import Fluid.Gen.Plan (Plan)
import Fluid.Gen.Lines (linesContent, line)

gen :: Array Plan -> Array String -> String
gen plan addonNames = linesContent do
  line ""
