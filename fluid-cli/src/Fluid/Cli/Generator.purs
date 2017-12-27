module Fluid.Cli.Generator where

import Prelude (Unit, bind, map, (==), (<>), (-))

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Data.Array as Array
import Data.Foldable (elem, or, foldr)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Path.Pathy (FileName(..), extension)
import Data.Traversable (traverse_, traverse)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, FS)

import Fluid.Gen.Version (applyVersionsFromSpec, applyVersionsFromSpecs)
import Fluid.Gen.Spec (parseSpecs, parseSpec)
import Fluid.Gen.Plan (Plan, PlanError, plan)
import Fluid.Gen.History (createHistory)
import Fluid.Gen.Blueprint (mkBlueprint, Blueprint)

import Fluid.Cli.Args
import Fluid.Cli.Target (writeTarget, Target)

type Generator
  =  Args
  -> String -- All Specs as a stringified JSON
  -> Array Blueprint
  -> Either (Array String) (Array Target)

generate :: forall eff. Args -> Generator -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generate args generator =
  -- This function is fairly hairy,
  -- but it could be simplified with a single JSON library for reading, writing, and modifying.
  if hasJsonExtension args.src
    then do
      -- If all the specs are in a single file
      contents <- readTextFile UTF8 args.src
      case parseSpecs contents of
        Left e -> log e
        Right specs -> do
          -- Create first histories to generate json specs with versions
          let histories = createHistory specs
          case applyVersionsFromSpec (map (\{version} -> version) histories) contents of
            Left e -> log e
            Right t -> go t histories
    else do
      -- If the specs are separated in different files.
      items' <- readdir args.src
      let items = Array.filter hasJsonExtension items'
      contents <- traverse (\item -> readTextFile UTF8 (args.src <> "/" <> item)) items
      case traverse parseSpec contents of
        Left e -> log e
        Right specs -> do
          let histories = createHistory specs
          case applyVersionsFromSpecs (Array.zip (map (\{version} -> version) histories) contents) of
            Left e -> log e
            Right t -> go t histories
  where
    go (Tuple jsonSpecsAsOne jsonSpecs) histories = do
      let blueprints = Array.zipWith mkBlueprint histories jsonSpecs
      case generator args jsonSpecsAsOne blueprints of
        Left errors -> traverse_ log errors
        Right targets -> traverse_ writeTarget targets

hasJsonExtension :: String -> Boolean
hasJsonExtension s = extension (FileName s) == "json"

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
