module Fluid.Cli.Ruby where

import Prelude (map, (<>), show, ($), bind, pure)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Plan (Plan, PlanError, plan)
import Fluid.Gen.Ruby.ServerVersion as Server
import Fluid.Gen.Ruby.ServerLatest as ServerLatest

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)
import Fluid.Cli.Generator (Generator, planFrom, separate)

generateRubyServer :: Generator
generateRubyServer args jsonSpec blueprints = lmap (map show) $ do
  planTargets <- separate $ map
    (\bp -> map
      (\p -> Tuple p {path: buildPath ("major" <> show bp.version.major <> ".rb"), contents: Server.gen p args.addon})
      (planFrom args bp))
    blueprints
  let plans = map fst planTargets  :: Array Plan
  let versionTargets = map snd planTargets :: Array Target
  let latestTarget = { path: buildPath "server.rb", contents: ServerLatest.gen plans args.addon }
  pure $ Array.cons latestTarget versionTargets
  where
    buildPath path = args.dest <> "/" <> args.name <> "/" <> path
