module Fluid.Cli.Node where

import Prelude ((<>), show)
import Fluid.Gen.Node.ServerVersion as Server
import Fluid.Gen.Node.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".js") "Server.js" Server.gen ServerLatest.gen
