module Fluid.Cli.Haskell where

import Prelude ((-), map, (<>), show, ($))

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, and, foldr)
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Haskell.Spec (Plan, PlanError, plan)
import Fluid.Gen.Haskell.Server as Server

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)
import Fluid.Cli.Generator (Generator)

generateHaskellServer :: Generator
generateHaskellServer args jsonSpec blueprints = lmap (map show) $ separate $ map
  (\bp -> map
    (\p -> {path: buildPath ("V" <> show bp.version.major <> ".hs"), contents: Server.gen p args.addon})
    (planFrom args bp))
  blueprints
  where
    buildPath path = args.dest <> "/" <> path

separate :: forall e a. Array (Either e a) -> Either (Array e) (Array a)
separate xs = foldr go (Right []) xs
  where
    go e arrE = case arrE of
      Left ls -> case e of
        Left l -> Left (Array.cons l ls)
        Right _ -> Left ls
      Right rs -> case e of
        Left l -> Left (Array.singleton l)
        Right r -> Right (Array.cons r rs)

planFrom :: Args -> Blueprint -> Either PlanError Plan
planFrom args bp = let
  major = bp.version.major
  prevMajor = major - 1
  typeVersionMapper typeName =
    if and [elem typeName bp.diff.addType, elem typeName bp.diff.removeType, elem typeName bp.diff.modifyType]
      then major
      else prevMajor
  in plan args.prefix bp.version bp.spec args.addon typeVersionMapper bp.stringSpec
