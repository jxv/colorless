module Fluid.Cli.PureScript where

import Prelude (map, (<>), show, ($), bind, pure)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Gen.PureScript.ServerVersion as Server
import Fluid.Gen.PureScript.ServerLatest as ServerLatest
import Fluid.Gen.PureScript.Client as Client

import Fluid.Cli.Generator (Generator, planFrom, separate)
import Fluid.Cli.Generator as Gen

generateServer :: Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".purs") "Server.purs" Server.gen ServerLatest.gen

generateClient :: Generator
generateClient conv args depFilter jsonSpec blueprints = lmap (map show) $ do
  plans <- separate $ map (\bp -> planFrom conv args depFilter bp) blueprints
  case Array.head plans of
    Nothing -> pure []
    Just p -> do
      let target = { path: args.dest <> "/" <> args.name <> "/Client.purs", contents: Client.gen p args.addon }
      pure [target]
