module Fluid.Cli.Rust where

import Prelude ((<>), show)
import Fluid.Gen.Rust.ServerVersion as Server
import Fluid.Gen.Rust.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "major" <> show major <> ".rs") "server.rs" Server.gen ServerLatest.gen
