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
  ( version
  , handleRequest
  , ServiceThrower(..)
  , Service(..)
  , Hello(..)
  ) where

-- Imports
import qualified Prelude as P
import qualified Data.Map as Map
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Conversions as T
import qualified Data.String as P (IsString)
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified GHC.Generics as P (Generic)
import qualified Colorless.Types as C
import qualified Colorless.Runtime.Expr as C
import qualified Colorless.Runtime.Val as C (ToVal(..), FromVal(..), getMember, fromValFromJson, combineObjects)


-- Version
version :: C.Version
version = C.Version 0 1

-- ServiceThrower
class P.Monad m => ServiceThrower m where
  serviceThrow :: () -> m a

-- Service
class ServiceThrower m => Service meta m where
  goodbye :: meta -> m ()
  hello :: meta -> Hello -> m T.Text

-- Handle Request
handleRequest :: (Service meta m, C.RuntimeThrower m, IO.MonadIO m) => C.Options -> (() -> m meta) -> C.Request -> m C.Response
handleRequest options metaMiddleware C.Request{meta,calls} = do
  meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
  xformMeta <- metaMiddleware meta'
  envRef <- IO.liftIO C.emptyEnv
  variableBaseCount <- IO.liftIO (Map.size P.<$> IO.readIORef envRef)
  let options' = C.Options
        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)
        }
  let evalConfig = C.EvalConfig
        { C.options = options'
        , C.apiCall = api xformMeta
        }
  calls' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableCalls) P.return (P.mapM C.jsonToExpr calls)
  vals <- P.mapM (\v -> C.runEval (C.forceVal P.=<< C.eval v envRef) evalConfig) calls'
  P.return (C.Response'Success (A.toJSON vals))

-- API
api :: (Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
api meta' apiCall' = case C.parseApiCall apiParser apiCall' of
  P.Nothing -> C.runtimeThrow C.RuntimeError'UnrecognizedCall
  P.Just x' -> case x' of
    HelloWorld'Goodbye -> C.toVal P.<$> goodbye meta'
    HelloWorld'Hello a' -> C.toVal P.<$> hello meta' a'

-- API Parser
apiParser :: C.ApiParser HelloWorld
apiParser = C.ApiParser
  { hollow = Map.fromList
     [ ("Goodbye", HelloWorld'Goodbye)
     ]
  , struct = Map.fromList
     [ ("Hello", v HelloWorld'Hello)
     ]
  , enumeration = Map.empty
  , wrap = Map.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- API: HelloWorld
data HelloWorld
  = HelloWorld'Goodbye
  | HelloWorld'Hello Hello
  deriving (P.Show, P.Eq)

-- Struct: Hello
data Hello = Hello
  { target :: T.Text
  } deriving (P.Show, P.Eq, P.Generic)

instance A.ToJSON Hello

instance C.ToVal Hello where
  toVal Hello
    { target
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("target", C.toVal target)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> Hello
      P.<$> C.getMember m "target"
    _ -> P.Nothing

