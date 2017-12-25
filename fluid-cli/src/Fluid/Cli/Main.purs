module Fluid.Cli.Main where

import Prelude (Unit, bind, pure, unit, (&&), (==), discard)
import Control.Monad.Aff (Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Node.FS.Aff (FS)

import Fluid.Cli.Args
import Fluid.Cli.Haskell (generateHaskellServer, generateHaskellClient)
import Fluid.Cli.Generator (generate)

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = launchAff do
  args <- liftEff' getArgs
  if args.lang == "haskell" && args.side == "server"
    then generate args generateHaskellServer else pure unit
  if args.lang == "haskell" && args.side == "client"
    then generate args generateHaskellClient else pure unit
