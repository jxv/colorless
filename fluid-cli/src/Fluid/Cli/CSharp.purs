module Fluid.Cli.CSharp where

import Prelude ((<>), show)
import Fluid.Gen.CSharp.ServerVersion as Server
import Fluid.Gen.CSharp.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".cs") "Server.cs" Server.gen ServerLatest.gen
