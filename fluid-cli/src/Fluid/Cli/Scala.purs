module Fluid.Cli.Scala where

import Prelude ((<>), show)
import Fluid.Gen.Scala.ServerVersion as Server
import Fluid.Gen.Scala.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".scala") "Server.scala" Server.gen ServerLatest.gen
