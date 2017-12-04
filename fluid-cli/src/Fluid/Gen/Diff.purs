module Fluid.Gen.Diff where

import Prelude
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Array as Array

import Fluid.Gen.Spec

type Diff =
  { addType :: Array String
  , removeType :: Array String
  , modifyType :: Array String
  }

diff :: Spec -> Spec -> Diff
diff prevSpec nextSpec =
  { addType: []
  , removeType: []
  , modifyType: []
  }
