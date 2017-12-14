module Main where

import Prelude

import Control.Monad.Aff (Fiber)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Node.FS.Aff (FS)

import Fluid.Cli.Main as Cli

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = Cli.main
