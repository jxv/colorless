module Fluid.Cli.Ruby where

import Prelude ((<>), show)
import Fluid.Gen.Ruby.ServerVersion as Server
import Fluid.Gen.Ruby.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "major" <> show major <> ".rb") "server.rb" Server.gen ServerLatest.gen
