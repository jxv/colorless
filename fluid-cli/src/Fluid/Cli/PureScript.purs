module Fluid.Cli.PureScript where

import Prelude (map, (<>), show, ($), bind, pure, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Cli.Generator (Generator, planFrom, separate)
import Fluid.Cli.Args (Args)
import Fluid.Cli.Generator as Gen
import Fluid.Gen.PureScript.Conversion (conversion)
import Fluid.Gen.PureScript.ServerVersion as Server
import Fluid.Gen.PureScript.ServerLatest as ServerLatest
import Fluid.Gen.PureScript.Client as Client

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "Major" <> show major <> ".purs") "Server.purs" Server.gen ServerLatest.gen)

generateClient :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateClient args = Gen.generate conversion args Set.empty client
  where
    client :: Generator
    client conv args depFilter jsonSpec blueprints = lmap (map show) $ do
      plans <- separate $ map (\bp -> planFrom conv args depFilter bp) blueprints
      case Array.head plans of
        Nothing -> pure []
        Just p -> do
          let target = { path: args.dest <> "/" <> args.name <> "/Client.purs", contents: Client.gen p args.addon }
          pure [target]
