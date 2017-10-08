-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module
module HelloWorld.V0
  ( helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Handler
  , helloWorld'Spec
  , HelloWorld'Thrower(..)
  , HelloWorld'Service(..)
  , Hello(..)
  , helloWorld'Scotty'SendResponse
  , helloWorld'Scotty'GetSpec
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Except as M
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)

import qualified Colorless.Imports as R
import qualified Colorless.Server as C


import qualified Colorless.Server.Scotty as Scotty

-- Version
helloWorld'Version :: C.Version
helloWorld'Version = C.Version 0 1

helloWorld'Pull :: C.Pull
helloWorld'Pull = C.Pull "http" "127.0.0.1" "/" 8080

-- Thrower
class C.ServiceThrower m => HelloWorld'Thrower m where
  helloWorld'Throw :: () -> m a
  helloWorld'Throw = C.serviceThrow P.. R.toJSON

-- Service
class P.Monad m => HelloWorld'Service meta m where
  goodbye :: meta -> m ()
  hello :: meta -> Hello -> m R.Text

instance HelloWorld'Service meta m => HelloWorld'Service meta (M.ExceptT C.Response m) where
  goodbye _meta = M.lift  P.$ goodbye _meta
  hello _meta = M.lift  P.. hello _meta

helloWorld'Scotty'SendResponse
  :: (Scotty.ScottyError e, R.MonadIO m, HelloWorld'Service meta m)
  => C.Options
  -> (() -> m meta)
  -> C.Pull
  -> Scotty.ScottyT e m ()
helloWorld'Scotty'SendResponse _options _metaMiddleware _pull = Scotty.sendResponseSingleton _pull helloWorld'Version (helloWorld'Handler _options _metaMiddleware)

helloWorld'Scotty'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
helloWorld'Scotty'GetSpec = Scotty.getSpec P.$ R.toJSON [helloWorld'Spec]

-- Handler
helloWorld'Handler
  :: (HelloWorld'Service meta m, R.MonadIO m)
  => C.Options
  -> (() -> m meta)
  -> C.Request
  -> m (P.Either C.Response C.Response)
helloWorld'Handler options metaMiddleware C.Request{meta,query} = M.runExceptT P.$ do
  meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
  xformMeta <- M.lift P.$ metaMiddleware meta'
  envRef <- R.liftIO C.emptyEnv
  variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)
  let options' = C.Options
        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)
        }
  let evalConfig = C.EvalConfig
        { C.options = options'
        , C.apiCall = helloWorld'ApiCall xformMeta
        }
  query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
  vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
  P.return (C.Response'Success (R.toJSON vals))

-- API
helloWorld'ApiCall :: (HelloWorld'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
helloWorld'ApiCall meta' apiCall' = case C.parseApiCall helloWorld'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow C.RuntimeError'UnrecognizedCall
  P.Just x' -> case x' of
    HelloWorld'Api'Goodbye -> C.toVal P.<$> goodbye meta'
    HelloWorld'Api'Hello a' -> C.toVal P.<$> hello meta' a'

-- API Parser
helloWorld'ApiParser :: C.ApiParser HelloWorld'Api
helloWorld'ApiParser = C.ApiParser
  { hollow = R.fromList
     [ ("Goodbye", HelloWorld'Api'Goodbye)
     ]
  , struct = R.fromList
     [ ("Hello", v HelloWorld'Api'Hello)
     ]
  , enumeration = R.empty
  , wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data HelloWorld'Api
  = HelloWorld'Api'Goodbye
  | HelloWorld'Api'Hello Hello
  deriving (P.Show, P.Eq)

-- Struct: Hello
data Hello = Hello
  { target :: R.Text
  } deriving (P.Show, P.Eq)

instance C.ToVal Hello where
  toVal Hello
    { target
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("target", C.toVal target)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Hello
      P.<$> C.getMember _m "target"
    _ -> P.Nothing

instance R.ToJSON Hello where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Hello where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

helloWorld'Spec :: R.Value
helloWorld'Spec = v
  where P.Just v = R.decode "{\"colorless\":{\"major\":0,\"minor\":0},\"version\":{\"major\":0,\"minor\":1},\"types\":[{\"n\":\"Hello\",\"m\":[{\"target\":\"String\"}],\"o\":\"String\"},{\"n\":\"Goodbye\",\"o\":\"Unit\"}],\"pull\":{\"protocol\":\"http\",\"name\":\"HelloWorld\",\"host\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/\",\"port\":8080,\"error\":\"Unit\"}}"

