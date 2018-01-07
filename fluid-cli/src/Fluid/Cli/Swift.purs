module Fluid.Cli.Swift where

import Prelude ((<>), show)
import Fluid.Gen.Swift.ServerVersion as Server
import Fluid.Gen.Swift.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".swift") "Server.swift" Server.gen ServerLatest.gen
