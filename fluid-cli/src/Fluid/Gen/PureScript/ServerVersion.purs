module Fluid.Gen.PureScript.ServerVersion where

import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_)
import Fluid.Gen.PureScript.Common
import Fluid.Gen.Plan (Enumeral, Enumeration, Func, Plan, Struct, Wrap, lowercaseFirstLetter, uppercaseFirstLetter, PullPlan)
import Fluid.Gen.Lines (Lines, addLine, line, lines, linesContent, lineList)
import Fluid.Gen.Spec (Version)
import Prelude (Unit, discard, flip, map, show, ($), (<>), (/=), (==), pure, unit)

mkImportTypes :: Plan -> Array { name :: String, major :: Int }
mkImportTypes plan = Array.filter (\a -> a.major /= plan.version.major) $
  map (\w -> { name: w.name, major: w.major }) plan.wraps <>
  map (\s -> { name: s.name, major: s.major }) plan.structs <>
  Array.concatMap
    (\e ->
      [{ name: e.name, major: e.major }] <>
      Array.catMaybes (map (enumeralImport e.name e.major) e.enumerals))
    plan.enumerations

enumeralImport :: String -> Int -> Enumeral -> Maybe { name :: String, major :: Int }
enumeralImport name major enumeral = case enumeral.members of
  Nothing -> Nothing
  Just _ -> Just { name: enumeralNameTagMember name enumeral.tag, major }

genImports :: { prefix :: String, imports :: Array { name :: String, major :: Int }, importing :: Array (Lines Unit) } -> Lines Unit
genImports {prefix, imports, importing} = do
  line ""
  lines
    [ "-- Imports"
    , "import Prelude as P"
    , "import Control.Monad as P"
    , "import Control.Monad.Except as M"
    , "import Data.IORef as IO"
    , "import Data.String as P (IsString)"
    , "import Fluid.Imports as R"
    , "import Fluid.Server as C" ]
  traverse_ addLine $ map
    (\{name, major} -> ["import ", prefix, ".V", show major, " (", name, "(..))"])
    imports
  sequence_ importing

mkApiCalls :: Plan -> Array { name :: String, filled :: Boolean }
mkApiCalls plan = Array.concat
  [ mk plan.hollows false
  , mk (filterFunc plan.wraps) true
  , mk (filterFunc plan.structs) true
  , mk (filterFunc plan.enumerations) true
  ]
  where
    mk :: forall a. Array { name :: String | a } -> Boolean -> Array { name :: String, filled :: Boolean }
    mk xs b = map (\ty -> { name: ty.name, filled: b }) xs

genApi :: { name :: String, calls :: Array { name :: String, filled :: Boolean } } -> Lines Unit
genApi {name,calls} = do
  line ""
  line "-- Api"
  addLine ["data ", name, "'Api"]
  lineList calls
    "  = "
    "  | "
    (\call -> [name, "'Api'", call.name, if call.filled then " " <> call.name else ""])

genThrower :: { name :: String, lowercase :: String, error :: String } -> Lines Unit
genThrower {name, lowercase, error} = do
  line ""
  line "-- Thrower"
  addLine ["class C.ServiceThrower m <= ", name, "'Thrower m where"]
  addLine ["  ", lowercase, "'throw :: ", error, " -> m a"]
  addLine ["  ", lowercase, "'throw = C.serviceThrow P.. R.toJSON P.. C.toVal"]

mkServiceCalls :: Plan -> Array { name :: String, lowercase :: String, output :: String, hollow :: Boolean }
mkServiceCalls plan = Array.concat
  [ map (\x -> { name: x.name, lowercase: x.func.name, output: x.func.output, hollow: true }) plan.hollows
  , mkCall plan.wraps
  , mkCall plan.structs
  , mkCall plan.enumerations ]
  where
    mkCall :: forall a. Array { name :: String, func :: Maybe Func | a } -> Array { name :: String, lowercase :: String, output :: String, hollow :: Boolean }
    mkCall types = Array.catMaybes (map extractFunc types)
      where
        extractFunc x = case x.func of
          Nothing -> Nothing
          Just func -> Just { lowercase: func.name, name: x.name, output: func.output, hollow: false }

genService :: { name :: String, lowercase :: String, calls :: Array { lowercase :: String, name :: String, output :: String, hollow :: Boolean } } -> Lines Unit
genService {name, lowercase, calls} = do
  line ""
  line "-- Service"
  addLine ["class P.Monad m <= ", name, "'Service meta m where"]
  flip traverse_ calls $ \call ->
    addLine ["  ", lowercase, "'", call.name, " :: meta ->", if call.hollow then  " " else " " <> call.name <> " -> ", "m ", call.output]
  line ""
  addLine ["instance ", name, "'Service meta m => ", name, "'Service meta (M.ExceptT C.Response m) where"]
  flip traverse_ calls $ \call ->
    addLine ["  ", lowercase, "'", call.name, " _meta = M.lift", if call.hollow then " P.$ " else " P.. ", lowercase, "'", call.name, " _meta"]

mkApiParserCalls
  :: Plan
  ->  { hollow :: Array { label :: String, name :: String }
      , struct :: Array { label :: String, name :: String }
      , enumeration :: Array { label :: String, name :: String }
      , wrap :: Array { label :: String, name :: String } }
mkApiParserCalls plan =
  { hollow: map f plan.hollows
  , struct: map f $ filterFunc plan.structs
  , enumeration: map f $ filterFunc plan.enumerations
  , wrap: map f $ filterFunc plan.wraps }
  where
    f :: forall a. { name :: String, label :: String | a } -> { name :: String, label :: String }
    f ty = { name: ty.name, label: ty.label }

genApiParser
  :: { name :: String
     , lowercase :: String
     , calls ::
        { hollow :: Array { label :: String, name :: String }
        , struct :: Array { label :: String, name :: String }
        , enumeration :: Array { label :: String, name :: String }
        , wrap :: Array { label :: String, name :: String } } }
  -> Lines Unit
genApiParser {name, lowercase, calls} = do
  line ""
  line "-- API Parser"
  addLine [lowercase, "'ApiParser :: C.ApiParser ", name, "'Api"]
  addLine [lowercase, "'ApiParser = C.ApiParser"]
  -- Hollow
  case Array.uncons calls.hollow of
    Nothing -> line "  { C.hollow = R.empty"
    Just {head, tail} -> do
      line "  { C.hollow = R.fromList"
      addLine ["     [ (\"", head.label, "\", ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", ", name, "'Api'", item.name, ")"]
      line "     ]"
  -- Struct
  case Array.uncons calls.struct of
    Nothing -> line "  , C.struct = R.empty"
    Just {head, tail} -> do
      line "  , C.struct = R.fromList"
      addLine ["     [ (\"", head.label, "\", v ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", v ", name, "'Api'", item.name, ")"]
      line "     ]"
  -- Enumeration
  case Array.uncons calls.enumeration of
    Nothing -> line "  , C.enumeration = R.empty"
    Just {head, tail} -> do
      line "  , C.enumeration = R.fromList"
      addLine ["     [ (\"", head.label, "\", v ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", v ", name, "'Api'", item.name, ")"]
      line "     ]"
  -- Wrap
  case Array.uncons calls.wrap of
    Nothing -> line "  , C.wrap = R.empty"
    Just {head, tail} -> do
      line "  , C.wrap = R.fromList"
      addLine ["     [ (\"", head.label, "\", v ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", v ", name, "'Api'", item.name, ")"]
      line "     ]"
  line "  }"
  line "  where"
  line "    v x y = x P.<$> C.fromVal y"

mkApiLookupPairs :: Plan -> { hollow :: Array { name :: String }, filled :: Array { name :: String } }
mkApiLookupPairs plan =
  { hollow: map (\x -> { name: x.name }) plan.hollows
  , filled: Array.concat [ nameOfFunc plan.wraps, nameOfFunc plan.structs, nameOfFunc plan.enumerations ] }
  where
    nameOfFunc :: forall a. Array { name :: String, func :: Maybe Func | a } ->  Array { name :: String }
    nameOfFunc xs = map (\x -> { name: x.name }) $ Array.filter (\x -> isJust x.func) xs

genApiLookup :: { name :: String, lowercase :: String, calls :: { hollow :: Array { name :: String }, filled :: Array { name :: String } } } -> Lines Unit
genApiLookup {name,lowercase,calls} = do
  line ""
  line "-- API"
  addLine [lowercase, "'ApiCall :: (", name, "'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val"]
  addLine [lowercase, "'ApiCall meta' apiCall' = case C.parseApiCall ", lowercase, "'ApiParser apiCall' of"]
  line "  P.Nothing -> C.runtimeThrow (C.RuntimeError'UnrecognizedCall P.$ C.apiCallName apiCall')"
  line "  P.Just x' -> case x' of"
  flip traverse_ calls.hollow $ \item ->
    addLine ["    ", name, "'Api'", item.name, " -> C.toVal P.<$> ", lowercase, "'", item.name, " meta'"]
  flip traverse_ calls.filled $ \item ->
    addLine ["    ", name, "'Api'", item.name, " a' -> C.toVal P.<$> ", lowercase, "'", item.name, " meta' a'"]

genHandlerRequest :: { name :: String, lowercase :: String, meta :: String } -> Lines Unit
genHandlerRequest {name,lowercase,meta} = do
  line ""
  line "-- Handler"
  addLine [lowercase, "'handler"]
  addLine ["  :: (", name, "'Service meta m, R.MonadIO m, R.MonadCatch m)"]
  addLine ["  => (xtra -> C.Hooks m ", meta, " meta)"]
  line "  -> xtra"
  line "  -> C.Request"
  line "  -> m (P.Either C.Response C.Response)"
  addLine [lowercase, "'handler _hooksBuilder xtra C.Request{C.meta=meta, C.query=query} = R.catch"]
  lines
    [ "  (M.runExceptT P.$ do"
    , "    meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)"
    , "    let _hooks = _hooksBuilder xtra"
    , "    xformMeta <- M.lift P.$ C.metaMiddleware _hooks meta'"
    , "    envRef <- R.liftIO C.emptyEnv"
    , "    variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)"
    , "    _limits <- M.lift P.$ C.sandboxLimits _hooks xformMeta"
    , "    let _limits' = _limits"
    , "          { C.variables = P.fmap (P.+ variableBaseCount) (C.variables _limits)"
    , "          }"
    , "    _serviceCallCountRef <- R.liftIO (IO.newIORef 0)"
    , "    _lambdaCountRef <- R.liftIO (IO.newIORef 0)"
    , "    _exprCountRef <- R.liftIO (IO.newIORef 0)"
    , "    let evalConfig = C.EvalConfig"
    , "          { C.limits = _limits'"
    , "          , C.langServiceCallCount = _serviceCallCountRef"
    , "          , C.langLambdaCount = _lambdaCountRef"
    , "          , C.langExprCount = _exprCountRef" ]
  addLine ["          , C.apiCall = ", lowercase, "'ApiCall xformMeta"]
  lines
    [ "          }"
    , "    query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)"
    , "    vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig"
    , "    P.return P.$ C.Response'Success (R.toJSON vals) _limits)"
    , "  (\\(C.ThrownValue _err) -> P.return P.. P.Left P.$ C.Response'Error (C.ResponseError'Service _err))" ]

genModule :: { name :: String, lowercase :: String, prefix :: String, version :: Version, types :: Array String, values :: Array String } -> Lines Unit
genModule {name, lowercase, prefix, version, types, values} = do
  line "-- Module"
  addLine ["module ", prefix , ".Major", show version.major]
  addLine ["  ( ", lowercase, "'version"]
  addLine ["  , ", lowercase, "'pull"]
  addLine ["  , ", lowercase, "'handler"]
  addLine ["  , ", lowercase, "'spec"]
  addLine ["  , ", name, "'Thrower(..)"]
  addLine ["  , ", name, "'Service(..)"]
  traverse_ addLine $ map (\ty -> ["  , ", ty, "(..)"]) types
  traverse_ addLine $ map (\value -> ["  , ", value]) values
  line "  ) where"

genSpec :: String -> String -> Lines Unit
genSpec lowercase spec = do
  line ""
  addLine [lowercase, "'spec :: R.Value"]
  addLine [lowercase, "'spec = v"]
  addLine ["  where P.Just v = R.decode ", show spec]

type Addon =
  { exporting :: Array String
  , importing :: Lines Unit
  , gen :: Lines Unit }

createAddon :: Plan -> String -> Maybe Addon
createAddon plan addon = case addon of
  _ -> Nothing

filterVersion :: forall a. Version -> Array { major :: Int | a } -> Array { major :: Int | a }
filterVersion version = Array.filter (\ty -> ty.major == version.major)

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do
  let exportTypes = mkExportTypes plan
  let importTypes = mkImportTypes plan
  let serviceCalls = mkServiceCalls plan
  let apiLookupPairs = mkApiLookupPairs plan
  let apiParserCalls = mkApiParserCalls plan
  let apiCalls = mkApiCalls plan
  let addons = Array.catMaybes $ map (createAddon plan) addonNames
  let addonExporting = Array.concatMap (\x -> x.exporting) addons
  let addonImporting = map (\x -> x.importing) addons
  let currentWraps = filterVersion plan.version plan.wraps
  let currentStructs = filterVersion plan.version plan.structs
  let currentEnumerations = filterVersion plan.version plan.enumerations

  genModule
    { name: plan.name
    , lowercase: plan.lowercase
    , prefix: plan.prefix
    , version: plan.version
    , types: exportTypes
    , values: addonExporting }
  genImports
    { prefix: plan.prefix
    , imports: importTypes
    , importing: addonImporting }

  line ""
  line "--------------------------------------------------------"
  line "-- Configs"
  line "--------------------------------------------------------"

  genVersion
    { lowercase: plan.lowercase
    , version: plan.version }
  genPull
    { lowercase: plan.lowercase
    , pull: plan.pull }

  line ""
  line "--------------------------------------------------------"
  line "-- Interfaces"
  line "--------------------------------------------------------"

  genThrower
    { name: plan.name
    , lowercase: plan.lowercase
    , error: plan.pull.error }
  genService
    { name: plan.name
    , lowercase: plan.lowercase
    , calls: serviceCalls }

  line ""
  line "--------------------------------------------------------"
  line "-- Types"
  line "--------------------------------------------------------"

  traverse_ genWrap currentWraps
  traverse_ genStruct currentStructs
  traverse_ genEnumeration currentEnumerations

  line ""
  line "--------------------------------------------------------"
  line "-- Add-ons"
  line "--------------------------------------------------------"

  traverse_ (\addon -> addon.gen) addons

  line ""
  line "--------------------------------------------------------"
  line "-- Request handling"
  line "--------------------------------------------------------"

  genHandlerRequest
    { name: plan.name
    , lowercase: plan.lowercase
    , meta: plan.pull.meta }
  genApiLookup
    { name: plan.name
    , lowercase: plan.lowercase
    , calls: apiLookupPairs }
  genApiParser
    { name: plan.name
    , lowercase: plan.lowercase
    , calls: apiParserCalls }
  genApi
    { name: plan.name
    , calls: apiCalls }

  line ""
  line "--------------------------------------------------------"
  line "-- Type Instances"
  line "--------------------------------------------------------"

  flip traverse_ currentWraps $ \ty -> do
    genWrapToVal ty
    genWrapFromVal ty
    genToJson ty
    genFromJson ty

  flip traverse_ currentStructs $ \ty -> do
    genStructToVal ty
    genStructFromVal ty
    genToJson ty
    genFromJson ty

  flip traverse_ currentEnumerations $ \ty -> do
    genEnumerationToVal ty
    genEnumerationFromVal ty
    genToJson ty
    genFromJson ty

  line ""
  line "--------------------------------------------------------"
  line "-- Spec"
  line "--------------------------------------------------------"

  genSpec plan.lowercase plan.spec

  line ""
