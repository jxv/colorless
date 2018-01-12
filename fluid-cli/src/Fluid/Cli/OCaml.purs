module Fluid.Cli.OCaml where

import Prelude ((<>), show, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.OCaml.Conversion (conversion)
import Fluid.Gen.OCaml.ServerVersion as Server
import Fluid.Gen.OCaml.ServerLatest as ServerLatest

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "major_" <> show major <> ".ml") "server.ml" Server.gen ServerLatest.gen)
