module Fluid.Cli.Swift where

import Prelude ((<>), show, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.Swift.Conversion (conversion)
import Fluid.Gen.Swift.Dependency (depFilter)
import Fluid.Gen.Swift.ServerVersion as Server
import Fluid.Gen.Swift.ServerLatest as ServerLatest

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args depFilter (Gen.generateServer (\major -> "Major" <> show major <> ".swift") "Server.swift" Server.gen ServerLatest.gen)
