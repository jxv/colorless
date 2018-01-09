module Fluid.Cli.JavaScript where

import Prelude ((<>), show, Unit, ($), map, bind, pure)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Maybe (Maybe(..))

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Cli.Generator (Generator, separate, planFrom)
import Fluid.Gen.JavaScript.Conversion (conversion)
import Fluid.Gen.JavaScript.Client as Client

generateClient :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateClient args = Gen.generate conversion args Set.empty client
  where
    client :: Generator
    client conv args depFilter jsonSpec blueprints = lmap (map show) $ do
      plans <- separate $ map (\bp -> planFrom conv args depFilter bp) blueprints
      case Array.head plans of
        Nothing -> pure []
        Just p -> do
          let target = { path: args.dest <> "/" <> args.name <> ".js", contents: Client.gen p args.addon }
          pure [target]
