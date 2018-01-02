module Fluid.Gen.PureScript.ServerLatest where

import Data.Foldable
import Fluid.Gen.PureScript.Common
import Fluid.Gen.Plan
import Fluid.Gen.Lines

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Foldable (sequence_)
import Prelude (Unit, discard, flip, map, show, ($), (<>))

genModule :: { prefix :: String, name :: String, lowercase :: String, major :: Int, exportTypes :: Array String, values :: Array String } -> Lines Unit
genModule { prefix, name, lowercase, major, exportTypes, values } = do
  line ""
  line "-- Module"
  addLine ["module ", prefix]
  addLine ["  ( ", lowercase, "'handlerMap"]
  addLine ["  , ", lowercase, "'spec"]
  flip traverse_ values $ \value -> do
    addLine ["  , ", value]
  flip traverse_ exportTypes $ \ty -> do
    addLine ["  , V", show major, ".", ty, "(..)"]
  addLine ["  , V", show major, ".", name, "'Service(..)"]
  addLine ["  , V", show major, ".", name, "'Thrower(..)"]
  addLine ["  , V", show major, ".", lowercase, "'pull"]
  line "  ) where"

genImporting :: { importing :: Array (Lines Unit) } -> Lines Unit
genImporting {importing} = do
  line ""
  line "import Prelude as P"
  line "import Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)"
  line "import Fluid.Imports as R"
  sequence_ importing

genVersionImports:: { plan :: Plan, values :: Array String } -> Lines Unit
genVersionImports {plan: p, values} = do
  addLine ["import ", p.prefix, ".Major", show p.version.major, " as V", show p.version.major]
  addLine ["  ( ", p.name, "'Service(..)"]
  addLine ["  , ", p.name, "'Thrower(..)"]
  addLine ["  , ", p.lowercase, "'handler"]
  addLine ["  , ", p.lowercase, "'version"]
  addLine ["  , ", p.lowercase, "'pull"]
  addLine ["  , ", p.lowercase, "'spec"]
  flip traverse_ values $ \ty ->
    addLine ["  , ", ty, "(..)"]
  line "  )"

genHandlerMap :: String -> Array Plan -> Lines Unit
genHandlerMap lowercase plans = do
  line ""
  addLine [lowercase, "'handlerMap"]
  line "  ::"
  line "    ( R.MonadIO m"
  line "    , R.MonadCatch m"
  flip traverse_ plans $ \p ->
    addLine ["    , V", show p.version.major, ".", p.name, "'Service meta", show p.version.major, " m"]
  line "    )"
  lineList plans
    "  => (xtra -> C.Hooks m "
    "  -> C.Hooks m "
    (\p -> [p.pull.meta, " meta", show p.version.major, ")"])
  line "  -> xtra"
  line "  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))"
  addLine $
    [lowercase, "'handlerMap"] <> map (\p -> " hooks" <> show p.version.major) plans <> [" xtra = R.fromList"]
  lineList plans
    "    [ "
    "    , "
    (\p -> ["(", show p.version.major, ", (", show p.version.minor, ", V", show p.version.major, ".", p.lowercase, "'handler hooks", show p.version.major, " xtra))"])
  line "    ]"

genPublicSpec :: { lowercase :: String, plans :: Array Plan } -> Lines Unit
genPublicSpec {lowercase, plans} = do
  line ""
  addLine [lowercase, "'spec :: R.Value"]
  addLine [lowercase, "'spec = R.toJSON"]
  lineList plans
    "  [ "
    "  , "
    (\p -> ["V", show p.version.major, ".", p.lowercase, "'spec"])
  line "  ]"

type Addon =
  { importing :: Lines Unit
  , exporting :: Array String
  , gen :: Lines Unit
  }

createAddon :: {head :: Plan, all :: Array Plan} -> String -> Maybe Addon
createAddon {head,all} key = map (\addon -> addon head all) $ StrMap.lookup key $ StrMap.fromFoldable []

parseAddons :: Plan -> Array Plan -> Array String -> Array Addon
parseAddons p plans addons = foldl (\arr key -> tryCons (createAddon { head: p, all: plans } key) arr) [] addons
  where
    tryCons :: forall a. Maybe a -> Array a -> Array a
    tryCons Nothing xs = xs
    tryCons (Just x) xs = Array.cons x xs

gen :: Array Plan -> Array String -> String
gen plans addons = case Array.head plans of
  Nothing -> ""
  Just p -> linesContent do
    let exportTypes = mkExportTypes p
    let addonOptions = parseAddons p plans addons
    let importing = map (\item -> item.importing) addonOptions
    let exporting = Array.concat $ map (\item -> item.exporting) addonOptions
    let addonGen = map (\item -> item.gen) addonOptions
    genModule
      { prefix: p.prefix
      , name: p.name
      , lowercase: p.lowercase
      , major: p.version.major
      , exportTypes
      , values: exporting }
    genImporting { importing }
    flip traverse_ plans $ \p' -> genVersionImports { plan: p', values: mkExportTypes p' }
    genHandlerMap p.lowercase plans
    genPublicSpec { lowercase: p.lowercase, plans }
    sequence_ addonGen
    line ""
