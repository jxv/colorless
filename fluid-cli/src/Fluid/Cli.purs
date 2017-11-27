module Fluid.Cli where

import Prelude

import Control.Monad.Aff (launchAff, Fiber)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, FS)

import Fluid.Spec (parseSpec)

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = launchAff do
  contents <- readTextFile UTF8 "./config.json"
  case parseSpec contents of
    Right config -> log $ show $ config.pull.protocol
    Left e -> log e
