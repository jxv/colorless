module Fluid.Cli.Cpp where

import Prelude ((<>), show, Unit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Generator as Gen
import Fluid.Cli.Args (Args)
import Fluid.Gen.Cpp.Conversion (conversion)
import Fluid.Gen.Cpp.ServerVersion as Server
import Fluid.Gen.Java.ServerLatest as ServerLatest

generateServer :: forall eff. Args -> Aff (fs :: FS, console :: CONSOLE | eff) Unit
generateServer args = Gen.generate conversion args Set.empty (Gen.generateServer (\major -> "Major" <> show major <> ".cpp") "Server.cpp" Server.gen ServerLatest.gen)
