-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Colorless.Examples.HelloWorld.V0
  ( helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Handler
  , helloWorld'Spec
  , HelloWorld'Thrower(..)
  , HelloWorld'Service(..)
  , Hello(..)
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)
import qualified GHC.Generics as P (Generic)
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Conversions as T
import qualified Colorless.Server as C


-- Version
helloWorld'Version :: C.Version
helloWorld'Version = C.Version 0 1

helloWorld'Pull :: C.Pull
helloWorld'Pull = C.Pull "http" "127.0.0.1" "/" 8080

-- Thrower
class P.Monad m => HelloWorld'Thrower m where
  helloWorld'Throw :: () -> m a

-- Service
class HelloWorld'Thrower m => HelloWorld'Service meta m where
  goodbye :: meta -> m ()
  hello :: meta -> Hello -> m T.Text

-- Handler
helloWorld'Handler :: (HelloWorld'Service meta m, C.RuntimeThrower m, IO.MonadIO m) => C.Options -> (() -> m meta) -> C.Request -> m C.Response
helloWorld'Handler options metaMiddleware C.Request{meta,query} = do
  meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
  xformMeta <- metaMiddleware meta'
  envRef <- IO.liftIO C.emptyEnv
  variableBaseCount <- IO.liftIO (Map.size P.<$> IO.readIORef envRef)
  let options' = C.Options
        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)
        }
  let evalConfig = C.EvalConfig
        { C.options = options'
        , C.apiCall = helloWorld'ApiCall xformMeta
        }
  query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
  vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
  P.return (C.Response'Success (A.toJSON vals))

-- API
helloWorld'ApiCall :: (HelloWorld'Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
helloWorld'ApiCall meta' apiCall' = case C.parseApiCall helloWorld'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow C.RuntimeError'UnrecognizedCall
  P.Just x' -> case x' of
    HelloWorld'Api'Goodbye -> C.toVal P.<$> goodbye meta'
    HelloWorld'Api'Hello a' -> C.toVal P.<$> hello meta' a'

-- API Parser
helloWorld'ApiParser :: C.ApiParser HelloWorld'Api
helloWorld'ApiParser = C.ApiParser
  { hollow = Map.fromList
     [ ("Goodbye", HelloWorld'Api'Goodbye)
     ]
  , struct = Map.fromList
     [ ("Hello", v HelloWorld'Api'Hello)
     ]
  , enumeration = Map.empty
  , wrap = Map.empty
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
  { target :: T.Text
  } deriving (P.Show, P.Eq, P.Generic)

instance C.ToVal Hello where
  toVal Hello
    { target
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("target", C.toVal target)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Hello
      P.<$> C.getMember _m "target"
    _ -> P.Nothing

instance A.ToJSON Hello where
  toJSON = A.toJSON P.. C.toVal

instance A.FromJSON Hello where
  parseJSON _v = do
    _x <- A.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

helloWorld'Spec :: A.Value
helloWorld'Spec = v
  where P.Just v = A.decode "{\"colorless\":{\"major\":0,\"minor\":0},\"types\":[{\"n\":\"Hello\",\"m\":[{\"target\":\"String\"}],\"o\":\"String\"},{\"n\":\"Goodbye\",\"o\":\"Unit\"}],\"pull\":{\"protocol\":\"http\",\"name\":\"HelloWorld\",\"address\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/\",\"port\":8080,\"error\":\"Unit\"},\"version\":{\"major\":0,\"minor\":1}}"

