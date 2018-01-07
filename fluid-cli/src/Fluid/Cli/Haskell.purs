module Fluid.Cli.Haskell where

import Prelude (map, (<>), show, ($), bind, pure)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Gen.Haskell.ServerVersion as Server
import Fluid.Gen.Haskell.ServerLatest as ServerLatest
import Fluid.Gen.Haskell.Client as Client
import Fluid.Cli.Generator as Gen
import Fluid.Cli.Generator (planFrom, separate)


generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".hs") "Server.hs" Server.gen ServerLatest.gen

generateClient :: Gen.Generator
generateClient conv args depFilter jsonSpec blueprints = lmap (map show) $ do
  plans <- separate $ map (\bp -> planFrom conv args depFilter bp) blueprints
  case Array.head plans of
    Nothing -> pure []
    Just p -> do
      let target = { path: args.dest <> "/" <> args.name <> "/Client.hs", contents: Client.gen p args.addon }
      pure [target]
