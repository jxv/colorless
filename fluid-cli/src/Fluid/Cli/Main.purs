module Fluid.Cli.Main where

import Prelude (Unit, bind, map, pure, unit, (&&), (==))

import Control.Monad.Aff (Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Path.Pathy (FileName(..), extension)
import Data.Traversable (traverse_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, FS)

import Fluid.Gen.Version (applyVersionsFromSpec)
import Fluid.Gen.Spec (parseSpecs)
import Fluid.Gen.History (createHistory)
import Fluid.Gen.Blueprint (mkBlueprint)

import Fluid.Cli.Args
import Fluid.Cli.Target (writeTarget)
import Fluid.Cli.Haskell (generateHaskellServer)

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = launchAff do
  args <- liftEff' getArgs
  if args.lang == "haskell" && args.side == "server"
    then do
      if hasJsonExtension args.src
        then do
          contents <- readTextFile UTF8 args.src
          case parseSpecs contents of
            Right specs -> do
              let histories = createHistory specs
              case applyVersionsFromSpec (map (\{version} -> version) histories) contents of
                Left error -> log error
                Right (Tuple jsonSpecsAsOne jsonSpecs) -> do
                  let blueprints = Array.zipWith mkBlueprint histories jsonSpecs
                  case generateHaskellServer args jsonSpecsAsOne blueprints of
                    Left errors -> traverse_ log errors
                    Right targets -> traverse_ writeTarget targets
            Left e -> log e
        else pure unit
    else pure unit

hasJsonExtension :: String -> Boolean
hasJsonExtension s = extension (FileName s) == "json"
