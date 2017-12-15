module Fluid.Cli.Haskell where

import Prelude ((-), map, flip)
import Data.Either (Either(..))
import Data.Foldable (elem, and)
import Data.Traversable (traverse)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Haskell.Spec (Plan, PlanError, plan)
import Fluid.Gen.Haskell.Server as Server

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)

generateHaskellServer
  :: Args
  -> String -- All Specs as a stringified JSON
  -> Array Blueprint
  -> Either (Array String) (Array Target)
generateHaskellServer args jsonSpec blueprints = do
  let serverContents = traverse (\bp -> map (flip Server.gen args.addon) (planFrom args bp)) blueprints :: Either PlanError (Array String)
  Right []

planFrom :: Args -> Blueprint -> Either PlanError Plan
planFrom args bp = let
  major = bp.version.major
  prevMajor = major - 1
  typeVersionMapper typeName =
    if and [elem typeName bp.diff.addType, elem typeName bp.diff.removeType, elem typeName bp.diff.modifyType]
      then major
      else prevMajor
  in plan args.prefix bp.version bp.spec args.addon typeVersionMapper bp.stringSpec
