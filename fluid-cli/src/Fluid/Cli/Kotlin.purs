module Fluid.Cli.Kotlin where

import Prelude ((<>), show, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.Kotlin.Conversion (conversion)
import Fluid.Gen.Kotlin.ServerVersion as Server
import Fluid.Gen.Kotlin.ServerLatest as ServerLatest

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "Major" <> show major <> ".kt") "Server.kt" Server.gen ServerLatest.gen)
