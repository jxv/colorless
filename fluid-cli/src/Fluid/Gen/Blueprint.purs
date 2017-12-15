module Fluid.Gen.Blueprint where

import Fluid.Gen.Diff (SchemaDiff)
import Fluid.Gen.Spec (Spec, Version)
import Fluid.Gen.History (History)

type Blueprint =
  { version :: Version
  , diff :: SchemaDiff
  , spec :: Spec
  , stringSpec :: String
  }

mkBlueprint :: History -> String -> Blueprint
mkBlueprint {version, spec, diff} stringSpec = { version, spec, diff, stringSpec }
