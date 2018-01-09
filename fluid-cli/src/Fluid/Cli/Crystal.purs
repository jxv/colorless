module Fluid.Cli.Crystal where

import Prelude ((<>), show)
import Fluid.Gen.Crystal.ServerVersion as Server
import Fluid.Gen.Crystal.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "major_" <> show major <> ".cr") "server.cr" Server.gen ServerLatest.gen
