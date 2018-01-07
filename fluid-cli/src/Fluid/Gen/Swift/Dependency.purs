module Fluid.Gen.Swift.Dependency where

import Data.Set as Set
import Fluid.Gen.Dependency

depFilter :: DepFilter
depFilter = Set.fromFoldable [DepTag'Direct]
