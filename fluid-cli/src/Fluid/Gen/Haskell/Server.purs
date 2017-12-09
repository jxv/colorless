module Fluid.Gen.Haskell.Server where

import Prelude (Unit, discard, flip, map, pure, show, unit, ($), (<>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Traversable (traverse_)
import Fluid.Gen.Haskell.Common
import Fluid.Gen.Lines
import Fluid.Gen.Spec (Schema)

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
  , "{-# LANGUAGE NoImplicitPrelude #-}"
  ]

genImports :: String -> Array { name :: String, major :: Int } -> Array String -> Lines Unit
genImports prefix imports importing = do
  line ""
  lines
    [ "-- Imports"
    , "import qualified Prelude as P"
    , "import qualified Control.Monad as P"
    , "import qualified Control.Monad.Except as M"
    , "import qualified Data.IORef as IO"
    , "import qualified Data.String as P (IsString)"
    , "import qualified Fluid.Imports as R"
    , "import qualified Fluid.Server as C"
    ]
  traverse_ addLine $ map
    (\{name, major} -> ["import ", prefix, ".V", show major, " (", name, "(..))"])
    imports
  traverse_ line importing

genThrower :: String -> String -> String -> Lines Unit
genThrower name lowercase error = do
  line ""
  line "-- Thrower"
  addLine ["class C.ServiceThrower m => ", name, "'Thrower m where"]
  addLine ["  ", lowercase, "'throw :: ", error, " -> m a"]
  addLine ["  ", lowercase, "'throw = C.serviceThrow P.. R.toJSON P.. C.toVal"]


genService :: String -> String -> Array { name :: String, output :: String, hollow :: Boolean } -> Lines Unit
genService name lowercase calls = do
  line ""
  line "-- Service"
  addLine ["class P.Monad m => ", name, "'Service meta m where"]
  flip traverse_ calls $ \call ->
    addLine ["  ", lowercase, "'", call.name, " :: meta ->", if call.hollow then " " <> call.name <> " ->" else "", call.output]
  line ""
  addLine ["instance ", name, "'Service meta m => ", name, "'Service meta (M.ExceptT C.Response m) where"]
  flip traverse_ calls $ \call ->
    addLine ["  ", lowercase, "'", call.name, " _meta = M.lift ", if call.hollow then " P.$ " else " P.. ", lowercase, "'", call.name, " _meta"]

genApiParser
  :: String
  -> String
  -> { hollow :: Array { label :: String, name :: String }
     , struct :: Array { label :: String, name :: String }
     , enumeration :: Array { label :: String, name :: String }
     , wrap :: Array { label :: String, name :: String }
     }
  -> Lines Unit
genApiParser name lowercase calls = do
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

genApiLookup :: String -> String -> { hollow :: Array { name :: String }, filled :: Array { name :: String } } -> Lines Unit
genApiLookup name lowercase calls = do
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

genHandlerRequest :: String -> String -> String -> Lines Unit
genHandlerRequest name lowercase meta = do
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
    , "          , C.langExprCount = _exprCountRef"
    ]
  addLine ["          , C.apiCall = ", lowercase, "'ApiCall xformMeta"]
  lines
    [ "          }"
    , "    query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)"
    , "    vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig"
    , "    P.return P.$ C.Response'Success (R.toJSON vals) _limits)"
    , "  (\\(C.ThrownValue _err) -> P.return P.. P.Left P.$ C.Response'Error (C.ResponseError'Service _err))"
    ]

genModule :: String -> String -> String -> { major :: Int, minor :: Int } -> Array String -> Array String -> Lines Unit
genModule name lowercase prefix version types values = do
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
genSpec lowercase schema = do
  line ""
  addLine [lowercase, "'spec :: R.Value"]
  addLine [lowercase, "'spec = v"]
  addLine ["  where P.Just v = R.decode ", show schema]
