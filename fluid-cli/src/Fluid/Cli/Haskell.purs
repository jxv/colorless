module Fluid.Cli.Haskell where

import Prelude ((<>), show, Unit, ($), map, bind, pure)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.Haskell.Conversion (conversion)
import Fluid.Gen.Haskell.ServerVersion as Server
import Fluid.Gen.Haskell.ServerLatest as ServerLatest
import Fluid.Gen.Haskell.Client as Client
import Fluid.Cli.Generator (planFrom, separate)


generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "Major" <> show major <> ".hs") "Server.hs" Server.gen ServerLatest.gen)

generateClient :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateClient args = Gen.generate conversion args Set.empty client
  where
    client :: Gen.Generator
    client conv args depFilter jsonSpec blueprints = lmap (map show) $ do
      plans <- separate $ map (\bp -> planFrom conv args depFilter bp) blueprints
      case Array.head plans of
        Nothing -> pure []
        Just p -> do
          let target = { path: args.dest <> "/" <> args.name <> "/Client.hs", contents: Client.gen p args.addon }
          pure [target]
