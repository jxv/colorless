module Fluid.Cli.JavaScript where

import Prelude
import Data.Foldable (elem, or, foldr)
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Fluid.Cli.Args (Args)
import Fluid.Cli.Target (Target)
import Fluid.Cli.Generator (Generator)

import Fluid.Gen.Blueprint (Blueprint)
import Fluid.Gen.Haskell.Spec (Plan, PlanError, plan)
import Fluid.Gen.JavaScript.Client as Client

generateJavaScriptClient :: Generator
generateJavaScriptClient args jsonSpec blueprints = lmap (map show) $ do
  plans <- separate $ map (\bp -> planFrom args bp) blueprints
  case Array.head plans of
    Nothing -> pure []
    Just p -> do
      let target = { path: args.dest <> "/" <> args.name <> ".hs", contents: Client.gen p args.addon }
      pure [target]

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
    if or [elem typeName bp.diff.addType, elem typeName bp.diff.removeType, elem typeName bp.diff.modifyType]
      then major
      else prevMajor
  in plan args.prefix bp.version bp.spec args.addon typeVersionMapper bp.stringSpec
