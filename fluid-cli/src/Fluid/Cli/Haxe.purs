module Fluid.Cli.Haxe where

import Prelude ((<>), show, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.Haxe.Conversion (conversion)
import Fluid.Gen.Haxe.ServerVersion as Server
import Fluid.Gen.Haxe.ServerLatest as ServerLatest

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "Major" <> show major <> ".hx") "Server.hx" Server.gen ServerLatest.gen)
