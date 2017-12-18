module Fluid.Gen.Haskell.ServerLatest where

import Prelude (Unit, discard, ($), flip, show, map, (<>))
import Data.Traversable (traverse_)

import Fluid.Gen.Lines
import Fluid.Gen.Haskell.Spec

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
    addLine ["  , ", show major, ".", ty, "(..)"]
  addLine ["  , V", show major, ".", name, "'Service(..)"]
  addLine ["  , V", show major, ".", name, "'Thrower(..)"]
  addLine ["  , V", show major, ".", lowercase, "'pull"]
  line "  ) where"

genImporting :: { importing :: Array String } -> Lines Unit
genImporting {importing} = do
  line ""
  line "import qualified Prelude as P"
  line "import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)"
  line "import quailfied Fluid.Imports as R"
  traverse_ line importing

genVersionImports:: { prefix :: String, name :: String, lowercase :: String, major :: Int, exportTypes :: Array String } -> Lines Unit
genVersionImports {prefix, name, lowercase, major, exportTypes} = do
  addLine ["import qualified ", prefix, ".V", show major, " as V", show major]
  addLine ["  ( ", name, "'Service(..)"]
  addLine ["  , ", name, "'Thrower(..)"]
  addLine ["  , ", lowercase, "'handler"]
  addLine ["  , ", lowercase, "'version"]
  addLine ["  , ", lowercase, "'pull"]
  addLine ["  , ", lowercase, "'spec"]
  flip traverse_ exportTypes $ \ty ->
    addLine ["  , ", ty, "(..)"]
  line "  )"

genHandlerMap :: String -> Array Plan -> Lines Unit
genHandlerMap lowercase plans = do
  line ""
  addLine [lowercase, "'handlerMap"]
  line "  ::"
  line "    ( R.MonadIO m"
  line "    , R.MonadIO m"
  flip traverse_ plans $ \p ->
    addLine ["   , V", show p.version.major, ".", p.name, "'Service meta", show p.version.major, " m"]
  line "    )"
  lineList plans
    "  => (xtra -> C.Hooks m "
    "  -> C.Hooks m "
    (\p -> [p.pull.meta, " meta", show p.version.major])
  line "  -> xtra"
  line "  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))"
  addLine $
    [lowercase, "'handlerMap"] <> map (\p -> " hooks " <> show p.version.major) plans <> [" xtra = R.fromList"]
  lineList plans
    "    [ "
    "    , "
    (\p -> ["(", show p.version.major, ", (", show p.version.minor, ", V", show p.version.major, ".", p.lowercase, "'handler hooks", show p.version.major, " xtra))"])
  line "    ]"

genPublicSpec :: { lowercase :: String, specs :: Array { major :: Int, lowercase :: String } } -> Lines Unit
genPublicSpec {lowercase, specs} = do
  line ""
  addLine [lowercase, "'spec :: R.Value"]
  addLine [lowercase, "'spec = R.toJSON"]
  lineList specs
    "  [ "
    "  , "
    (\spec -> ["V", show spec.major, ".", spec.lowercase, "'spec"])
  line "  ]"
