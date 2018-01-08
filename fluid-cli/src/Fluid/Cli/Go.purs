module Fluid.Cli.Go where

import Prelude ((<>), show)
import Fluid.Gen.Go.ServerVersion as Server
import Fluid.Gen.Go.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "major_" <> show major <> ".go") "server.go" Server.gen ServerLatest.gen
