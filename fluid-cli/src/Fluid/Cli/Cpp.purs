module Fluid.Cli.Cpp where

import Prelude ((<>), show)
import Fluid.Gen.Cpp.ServerVersion as Server
import Fluid.Gen.Java.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".cpp") "Server.cpp" Server.gen ServerLatest.gen
