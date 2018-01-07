module Fluid.Cli.Python where

import Prelude ((<>), show)
import Fluid.Gen.Python.ServerVersion as Server
import Fluid.Gen.Python.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "major" <> show major <> ".py") "server.py" Server.gen ServerLatest.gen
