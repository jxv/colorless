module Fluid.Cli.Clojure where

import Prelude ((<>), show, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.Clojure.Conversion (conversion)
import Fluid.Gen.Clojure.ServerVersion as Server
import Fluid.Gen.Clojure.ServerLatest as ServerLatest

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "major-" <> show major <> ".clj") "server.clj" Server.gen ServerLatest.gen)
