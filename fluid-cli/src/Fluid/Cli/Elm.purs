module Fluid.Cli.Elm where

import Prelude ((<>), show)
import Fluid.Gen.Elm.ServerVersion as Server
import Fluid.Gen.Elm.ServerLatest as ServerLatest
import Fluid.Cli.Generator as Gen

generateServer :: Gen.Generator
generateServer = Gen.generateServer (\major -> "Major" <> show major <> ".elm") "Server.elm" Server.gen ServerLatest.gen
