module Fluid.Cli.Haskell where

import Data.Tuple (Tuple)
import Data.Either (Either(..))

import Fluid.Gen.Spec (Spec)
import Fluid.Gen.Blueprint (Blueprint)

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)

generateHaskellServer
  :: Args
  -> String -- All Specs as a stringified JSON
  -> Array Blueprint
  -> Either (Array String) (Array Target)
generateHaskellServer args jsonSpec specs' = do
  Right []
