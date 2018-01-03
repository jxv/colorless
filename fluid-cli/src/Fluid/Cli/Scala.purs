module Fluid.Cli.Scala where

import Prelude (map, (<>), show, ($), bind, pure)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Plan (Plan, PlanError, plan)
import Fluid.Gen.Scala.ServerVersion as Server
import Fluid.Gen.Scala.ServerLatest as ServerLatest

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)
import Fluid.Cli.Generator (Generator, planFrom, separate)

generateServer :: Generator
generateServer conv args jsonSpec blueprints = lmap (map show) $ do
  planTargets <- separate $ map
    (\bp -> map
      (\p -> Tuple p {path: buildPath ("Major" <> show bp.version.major <> ".scala"), contents: Server.gen p args.addon})
      (planFrom conv args bp))
    blueprints
  let plans = map fst planTargets  :: Array Plan
  let versionTargets = map snd planTargets :: Array Target
  let latestTarget = { path: buildPath "Server.scala", contents: ServerLatest.gen plans args.addon }
  pure $ Array.cons latestTarget versionTargets
  where
    buildPath path = args.dest <> "/" <> args.name <> "/" <> path
