module Fluid.Cli.Args where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (example, usage)

type Args =
  { src :: String
  , dest :: String
  , name :: String
  , lang :: String
  , prefix :: String
  , side :: String
  , major :: Int
  , addon :: Array String
  }

mkArgs :: forall eff. String -> String -> String -> String -> String -> String -> Int -> Array String -> Eff (console :: Eff.CONSOLE, exception :: EXCEPTION | eff) Args
mkArgs src dest name lang prefix side major addon = pure { src, dest , name, lang, prefix, side, major, addon }

getArgs :: forall eff. Eff (console :: Eff.CONSOLE, exception :: EXCEPTION | eff) Args
getArgs = do
  let setup = usage "$0 -l <language> -s <source> -m <module-name> -d <destination-dir> -n <name> -e <side>"
             <> example "$0 -l haskell -s ./api -m App.Api -n Api -d ./library/App/Api -e server" ""
  runY setup $ mkArgs
    <$> yarg "s" ["src"] (Just "Directory of specs OR JSON containing array of specs") (Right "Required") true
    <*> yarg "d" ["dest"] (Just "Directory to generate code") (Right "Required") true
    <*> yarg "n" ["name"] (Just "Name of top level source file and directory") (Right "Required") true
    <*> yarg "l" ["lang"] (Just "Language of code") (Right "Required") true
    <*> yarg "m" ["prefix"] (Just "Prefix or module name") (Right "Required") true
    <*> yarg "e" ["side"] (Just "\'client\' or \'server\' side code or \'both\'") (Left "client") true
    <*> yarg "v" ["major"] (Just "Oldest supported major version") (Left 0) true
    <*> yarg "a" ["addon"] (Just "Add-on code for client-side or server-side. May require additional dependencies.") (Left []) true
