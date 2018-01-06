module Fluid.Gen.Blueprint where

import Prelude
import Data.Set as Set
import Data.Ordering (invert)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Fluid.Gen.Diff (SchemaDiff)
import Fluid.Gen.Dependency (DepGraph, buildDepGraph)
import Fluid.Gen.Spec (Spec, Schema, Version, TypeName, TypeDecl(..), Type(..), MemberDecl(..), Param(..), EnumDecl(..))
import Fluid.Gen.History (History)

type Blueprint =
  { version :: Version
  , diff :: SchemaDiff
  , spec :: Spec
  , stringSpec :: String
  , depGraph :: DepGraph
  }

mkBlueprint :: History -> String -> Blueprint
mkBlueprint {version, spec, diff} stringSpec = { version, spec, diff, stringSpec, depGraph: buildDepGraph spec.schema }
