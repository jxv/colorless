module Fluid.Cli.Clojure where

import Prelude ((<>), show)
import Fluid.Gen.Clojure.ServerVersion as Server
import Fluid.Gen.Clojure.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "major-" <> show major <> ".clj") "server.clj" Server.gen ServerLatest.gen
