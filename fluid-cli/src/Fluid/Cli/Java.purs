module Fluid.Cli.Java where

import Prelude ((<>), show)
import Fluid.Gen.Java.ServerVersion as Server
import Fluid.Gen.Java.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".java") "Server.java" Server.gen ServerLatest.gen
