module Fluid.Cli.Haskell where

import Prelude (map, (<>), show, ($), bind, pure)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Plan (Plan, PlanError, plan)
import Fluid.Gen.Haskell.ServerVersion as Server
import Fluid.Gen.Haskell.ServerLatest as ServerLatest
import Fluid.Gen.Haskell.Client as Client

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)
import Fluid.Cli.Generator (Generator, planFrom, separate)

generateHaskellServer :: Generator
generateHaskellServer args jsonSpec blueprints = lmap (map show) $ do
  planTargets <- separate $ map
    (\bp -> map
      (\p -> Tuple p {path: buildPath ("V" <> show bp.version.major <> ".hs"), contents: Server.gen p args.addon})
      (planFrom args bp))
    blueprints
  let plans = map fst planTargets  :: Array Plan
  let versionTargets = map snd planTargets :: Array Target
  let latestTarget = { path: args.dest <> "/" <> args.name <> ".hs", contents: ServerLatest.gen plans args.addon }
  pure $ Array.cons latestTarget versionTargets
  where
    buildPath path = args.dest <> "/" <> args.name <> "/" <> path

generateHaskellClient :: Generator
generateHaskellClient args jsonSpec blueprints = lmap (map show) $ do
  plans <- separate $ map (\bp -> planFrom args bp) blueprints
  case Array.head plans of
    Nothing -> pure []
    Just p -> do
      let target = { path: args.dest <> "/" <> args.name <> ".hs", contents: Client.gen p args.addon }
      pure [target]
