module Fluid.Cli.JavaScript where

import Prelude
import Data.Foldable (elem, or, foldr)
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)
import Fluid.Cli.Generator (Generator, separate, planFrom)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Plan (Plan, PlanError, plan)
import Fluid.Gen.JavaScript.Client as Client

generateClient :: Generator
generateClient conv args jsonSpec blueprints = lmap (map show) $ do
  plans <- separate $ map (\bp -> planFrom conv args bp) blueprints
  case Array.head plans of
    Nothing -> pure []
    Just p -> do
      let target = { path: args.dest <> "/" <> args.name <> ".js", contents: Client.gen p args.addon }
      pure [target]
