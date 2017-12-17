module Fluid.Gen.Haskell.Server where

import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_)
import Fluid.Gen.Haskell.Common (enumeralNameTagMember)
import Fluid.Gen.Haskell.Spec (Enumeral, Enumeration, Func, Plan, Struct, Wrap, lowercaseFirstLetter, uppercaseFirstLetter, PullPlan)
import Fluid.Gen.Lines (Lines, addLine, line, lines, linesContent)
import Fluid.Gen.Spec (Version)
import Prelude (Unit, discard, flip, map, show, ($), (<>), (/=), (==), pure, unit)

mkExportTypes :: Plan -> Array String
mkExportTypes plan =
  map (\x -> x.name) plan.wraps <>
  map (\x -> x.name) plan.structs <>
  Array.concat (flip map plan.enumerations $ \e ->
    [e.name] <>
    map
      (\enumeral -> enumeralNameTagMember plan.name enumeral.tag)
      (Array.filter (\enumeral -> isJust enumeral.members) e.enumerals))

mkImportTypes :: Plan -> Array { name :: String, major :: Int }
mkImportTypes plan = Array.filter (\a -> a.major /= plan.version.major) $
  map (\w -> { name: w.name, major: w.major }) plan.wraps <>
  map (\s -> { name: s.name, major: s.major }) plan.structs <>
  Array.concatMap
    (\e ->
      [{ name: e.name, major: e.major }] <>
      Array.catMaybes (map (enumeralImport e.name e.major) e.enumerals))
    plan.enumerations

memberName :: String -> String -> String
memberName name member = lowercaseFirstLetter name <> uppercaseFirstLetter member

enumeralImport :: String -> Int -> Enumeral -> Maybe { name :: String, major :: Int }
enumeralImport name major enumeral = case enumeral.members of
  Nothing -> Nothing
  Just _ -> Just { name: enumeralNameTagMember name enumeral.tag, major }

lineList :: forall a. Array a -> String -> String -> (a -> Array String) -> Lines Unit
lineList arr headPrefix tailPrefix f = case Array.uncons arr of
  Nothing -> pure unit
  Just {head,tail} -> do
    addLine $ [headPrefix] <> f head
    flip traverse_ tail $ \item -> addLine $ [tailPrefix] <> f item

genToJson :: forall a. { name :: String | a } -> Lines Unit
genToJson {name} = do
  line ""
  addLine ["instance R.ToJSON ", name, " where"]
  line "  toJSON = R.toJSON P.. C.toVal"

genFromJson :: forall a. { name :: String | a } -> Lines Unit
genFromJson {name} = do
  line ""
  addLine ["instance R.FromJSON ", name, " where"]
  lines
    [ "  parseJSON _v = do"
    , "    _x <- R.parseJSON _v"
    , "    case C.fromVal _x of"
    , "      P.Nothing -> P.mzero"
    , "      P.Just _y -> P.return _y" ]

genWrap :: Wrap -> Lines Unit
genWrap {name, type: type', label, instances: {text, number}} = do
  line ""
  addLine ["-- Wrap ", name]
  addLine ["newtype ", name, " = ", name, " ", type']
  addLine ["  deriving (P.Eq, P.Ord, ", if text then "P.IsString, R.ToText, " else "", if number then "P.Num, " else "", " P.Show)"]

genWrapToVal :: Wrap -> Lines Unit
genWrapToVal {name} = do
  line ""
  addLine ["instance C.ToVal ", name, " where"]
  addLine ["  toVal (", name, " _w) = C.toVal _w"]

genWrapFromVal :: Wrap -> Lines Unit
genWrapFromVal {name} = do
  line ""
  addLine ["instance C.FromVal ", name, " where"]
  addLine ["  fromVal _v = ", name, " P.<$> C.fromVal _v"]

genStruct :: Struct -> Lines Unit
genStruct {name, label, members} = do
  line ""
  addLine ["-- Struct ", name]
  addLine ["data ", name, " = ", name]
  lineList members
    "  { "
    "  , "
    (\m -> [lowercaseFirstLetter name <> uppercaseFirstLetter m.name, " :: ", m.type])
  line "  } deriving (P.Show, P.Eq)"

genStructToVal :: Struct -> Lines Unit
genStructToVal {name, members} = do
  line ""
  addLine ["instance C.ToVal ", name, " where"]
  addLine ["  toVal ", name]
  lineList members
    "    { "
    "    , "
    (\m -> [memberName name m.name])
  line "    } "
  line " = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList"
  lineList members
    "    [ "
    "    , "
    (\m -> ["(\"", m.label, "\", C.toVal ", memberName name m.name, ")"])
  line "    ]"
  line ""

genStructFromVal :: Struct -> Lines Unit
genStructFromVal {name,members} = do
  addLine ["instance C.FromVal ", name, " where"]
  line "  fromVal = \\case"
  addLine ["  C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) ->", name]
  lineList members
    "    P.<$>"
    "    P.<*>"
    (\m -> [" C.getMember _m \"", m.label, "\""])
  line "    _ -> P.Nothing"

genEnumeration :: Enumeration -> Lines Unit
genEnumeration {name, enumerals} = do
  line ""
  addLine ["-- Enumeration: ", name]
  addLine ["data ", name]
  lineList enumerals "  = " "  | " (\e -> [name, "'", e.tag, if isJust e.members then "'Members" else ""])
  addLine ["  deriving (P.Show, P.Eq)"]
  flip traverse_ enumerals $ \enumeral ->
    case enumeral.members of
      Nothing -> pure unit
      Just members -> do
        line ""
        addLine ["data ", name, "'", enumeral.tag, "'Members = ", name, "'", enumeral.tag, "'Members"]
        lineList members
          "  { "
          "  , "
          (\m -> [lowercaseFirstLetter(name <> "'" <> enumeral.tag <> m.name), " :: ", m.type])
        line "  } deriving (P.Show, P.Eq)"

genEnumerationToVal :: Enumeration -> Lines Unit
genEnumerationToVal {name,enumerals} = do
  line ""
  addLine ["instance C.ToVal ", name, " where"]
  line "  toVal = \\case"
  flip traverse_ enumerals $ \enumeral -> case enumeral.members of
    Nothing -> addLine ["    ", name, "'", enumeral.tag, " -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral \"", enumeral.label, "\" P.Nothing"]
    Just members -> do
      addLine ["    ", name, "'", enumeral.tag, " ", name, "'", enumeral.tag, "'Members"]
      lineList members
        "      { "
        "      , "
        (\m -> [memberName (name <> "'" <> enumeral.tag) m.name])
      addLine ["    } -> ", name, "'", enumeral.tag, " -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral \"", enumeral.label, "\" P.$ P.Just P.$ R.fromList"]
      lineList members
        "      [ "
        "      , "
        (\m -> ["(\"", m.label, "\", C.toVal ", memberName (name <> "'" <> enumeral.tag) m.name, ")"])
      line "      ] "

genEnumerationFromVal :: Enumeration -> Lines Unit
genEnumerationFromVal {name,enumerals} = do
  line ""
  addLine ["instance C.FromVal ", name, " where"]
  line "  fromVal = \\case"
  line "    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of"
  flip traverse_ enumerals $ \enumeral -> case enumeral.members of
    Nothing -> addLine ["      (\"", enumeral.label, "\", P.Nothing) -> P.Just ", name, "'", enumeral.tag]
    Just members -> do
      lineList members
        "          P.<$>"
        "          P.<*>"
        (\m -> [" C.getMember _m' \"", m.label, "\""])
  line "      _ -> P.Nothing"
  line "    _ -> P.Nothing"

genVersion :: { lowercase :: String, version :: Version } -> Lines Unit
genVersion {lowercase, version} = do
  line ""
  line "-- Version"
  addLine [lowercase, "'version :: C.Version"]
  addLine [lowercase, "'version = C.Version", show version.major, " ", show version.minor]

genPull :: { lowercase :: String, pull :: PullPlan } -> Lines Unit
genPull {lowercase, pull: {protocol, host, path, port}} = do
  line ""
  addLine [lowercase, "'pull :: C.Pull"]
  addLine [lowercase, "'pull = C.Pull \"", protocol, "\" \"", host, "\" \"", path, "\" ", show port]

genPragmas :: Lines Unit
genPragmas = lines
  [ "-- Pragmas"
  , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
  , "{-# LANGUAGE LambdaCase #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , "{-# LANGUAGE NamedFieldPuns #-}"
  , "{-# LANGUAGE TupleSections #-}"
  , "{-# LANGUAGE FlexibleContexts #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE ScopedTypeVariables #-}"
  , "{-# LANGUAGE NoImplicitPrelude #-}" ]

genImports :: { prefix :: String, imports :: Array { name :: String, major :: Int }, importing :: Array (Lines Unit) } -> Lines Unit
genImports {prefix, imports, importing} = do
  line ""
  lines
    [ "-- Imports"
    , "import qualified Prelude as P"
    , "import qualified Control.Monad as P"
    , "import qualified Control.Monad.Except as M"
    , "import qualified Data.IORef as IO"
    , "import qualified Data.String as P (IsString)"
    , "import qualified Fluid.Imports as R"
    , "import qualified Fluid.Server as C" ]
  traverse_ addLine $ map
    (\{name, major} -> ["import ", prefix, ".V", show major, " (", name, "(..))"])
    imports
  sequence_ importing

mkApiCalls :: Plan -> Array { name :: String, filled :: Boolean }
mkApiCalls plan = Array.concat [mk plan.hollows false, mk plan.wraps true, mk plan.structs true, mk plan.enumerations true]
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
  line "  deriving (P.Show, P.Eq)"

genThrower :: { name :: String, lowercase :: String, error :: String } -> Lines Unit
genThrower {name, lowercase, error} = do
  line ""
  line "-- Thrower"
  addLine ["class C.ServiceThrower m => ", name, "'Thrower m where"]
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
  addLine ["class P.Monad m => ", name, "'Service meta m where"]
  flip traverse_ calls $ \call ->
    addLine ["  ", lowercase, "'", call.lowercase, " :: meta ->", if call.hollow then " " <> call.name <> " -> " else "", call.output]
  line ""
  addLine ["instance ", name, "'Service meta m => ", name, "'Service meta (M.ExceptT C.Response m) where"]
  flip traverse_ calls $ \call ->
    addLine ["  ", lowercase, "'", call.lowercase, " _meta = M.lift ", if call.hollow then " P.$ " else " P.. ", lowercase, "'", call.name, " _meta"]

mkApiParserCalls
  :: Plan
  ->  { hollow :: Array { label :: String, name :: String }
      , struct :: Array { label :: String, name :: String }
      , enumeration :: Array { label :: String, name :: String }
      , wrap :: Array { label :: String, name :: String } }
mkApiParserCalls plan =
  { hollow: map f plan.hollows
  , struct: map f plan.structs
  , enumeration: map f plan.enumerations
  , wrap: map f plan.wraps }
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
    Nothing -> line "  { hollow = R.empty"
    Just {head, tail} -> do
      line "  { hollow = R.fromList"
      addLine ["     [ (\"", head.label, "\", ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", ", name, "'Api'", item.name, ")"]
      line "     ]"
  -- Struct
  case Array.uncons calls.struct of
    Nothing -> line "  , struct = R.empty"
    Just {head, tail} -> do
      line "  , struct = R.fromList"
      addLine ["     [ (\"", head.label, "\", v ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", v ", name, "'Api'", item.name, ")"]
      line "     ]"
  -- Enumeration
  case Array.uncons calls.enumeration of
    Nothing -> line "  , enumeration = R.empty"
    Just {head, tail} -> do
      line "  , enumeration = R.fromList"
      addLine ["     [ (\"", head.label, "\", v ", name, "'Api'", head.name, ")"]
      flip traverse_ tail $ \item ->
        addLine ["     , (\"", item.label, "\", v ", name, "'Api'", item.name, ")"]
      line "     ]"
  -- Wrap
  case Array.uncons calls.wrap of
    Nothing -> line "  , wrap = R.empty"
    Just {head, tail} -> do
      line "  , wrap = R.fromList"
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
  addLine [lowercase, "'handler _hooksBuilder xtra C.Request{meta,query} = R.catch"]
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
  line ""
  line "-- Module"
  addLine ["module ", prefix , ".V", show version.major]
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

scottyAddon :: Plan -> Addon
scottyAddon plan =
  { exporting: [ plan.lowercase <> "'Scotty'Post", plan.lowercase <> "'Scotty'Get" ]
  , importing: line "import qualified Fluid.Server.Scotty as Scotty"
  , gen: do
      line ""
      addLine [plan.lowercase, "'Scotty'Post"]
      addLine [ "  :: (Scotty.ScottyError e, R.MonadIO m, ", plan.name, "'Service meta m, R.MonadCatch m)"]
      addLine [ "   => ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m ", plan.pull.meta, " meta)" ]
      lines
        [ "  -> C.Pull"
        , "  -> Scotty.ScottyT e m ()" ]
      addLine [ plan.lowercase, "'Scotty'Post _hooks _pull = Scotty.respondSingleton _pull ", plan.lowercase, "'version (\\_xtra -> ", plan.lowercase, "'handler _hooks _xtra)" ]
      line ""
      addLine [ plan.lowercase, "'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()" ]
      addLine [ plan.lowercase, "'Scotty'Get = Scotty.getSpec P.$ R.toJSON [", plan.lowercase, "'spec]" ] }

createAddon :: Plan -> String -> Maybe Addon
createAddon plan addon = case addon of
  "scotty" -> Just (scottyAddon plan)
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

  genPragmas
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
  line "-- Request handlers"
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
