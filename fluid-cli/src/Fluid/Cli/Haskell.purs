module Fluid.Cli.Haskell where

import Data.Tuple (Tuple)
import Data.Either (Either(..))

import Fluid.Gen.Spec (Spec)

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)

generateHaskellServer :: Args -> String -> Array (Tuple Spec String) -> Either (Array String) (Array Target)
generateHaskellServer args jsonSpec specs' = Right []
