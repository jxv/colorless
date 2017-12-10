module Fluid.Cli where

import Prelude

import Control.Monad.Aff (Aff, Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (FileName(..), extension)
import Data.StrMap as StrMap
import Data.Traversable (traverse_)
import Fluid.Gen.Diff (Diff, diffs)
import Fluid.Gen.Spec (parseSpecs, Spec, TypeName, Version, Schema, Bridge)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile, FS)
import Node.Path (FilePath)
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
              case generateHaskellServer args specs of
                Left errors -> traverse_ log errors
                Right targets -> traverse_ writeTarget targets
            Left e -> log e
        else pure unit
    else pure unit

type Target =
  { path :: String
  , contents :: String
  }

generateHaskellServer :: Args -> Array Spec -> Either (Array String) (Array Target)
generateHaskellServer args specs = do
  let d = diffs $ Array.toUnfoldable (map (\spec -> spec.schema) specs)
  Right []

typeChanges :: Diff -> { major :: Array TypeName, minor :: Array TypeName }
typeChanges d =
  { major: Array.nub $ Array.concat [d.removeType, d.modifyType]
  , minor: d.addType
  }

data Delta
  = Delta'Major
  | Delta'Minor

hasJsonExtension :: String -> Boolean
hasJsonExtension s = extension (FileName s) == "json"

nextVersion :: Version -> Delta -> Version
nextVersion {major,minor} Delta'Major = { major: major + 1, minor }
nextVersion {major,minor} Delta'Minor = { major, minor: minor + 1 }

initBridge :: Schema -> Version -> Bridge
initBridge schema version = { version, types: StrMap.keys schema }

versionChange :: Diff -> Delta
versionChange d =
  if Array.null d.addType && Array.null d.removeType && Array.null d.modifyType
    then Delta'Minor
    else Delta'Major

--

writeTarget :: forall eff. Target -> Aff (fs :: FS | eff) Unit
writeTarget target = do
  writeUTF8File target.path target.contents

writeUTF8File :: forall eff. FilePath -> String -> Aff (fs :: FS | eff) Unit
writeUTF8File = writeTextFile UTF8
