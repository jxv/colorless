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
module Colorless.Examples.HelloWorld.V2
  ( version
  , handleRequest
  , ServiceThrower(..)
  , Service(..)
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
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

import Colorless.Examples.HelloWorld.V1 (Goodbye(..))
import Colorless.Examples.HelloWorld.V1 (Color(..))
import Colorless.Examples.HelloWorld.V1 (Color'Custom'Members(..))

-- Version
version :: C.Version
version = C.Version 2 0

-- ServiceThrower
class P.Monad m => ServiceThrower m where
  serviceThrow :: () -> m a

-- Service
class ServiceThrower m => Service meta m where
  hello :: meta -> Hello -> m T.Text
  goodbye :: meta -> Goodbye -> m ()

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
    HelloWorld'Hello a' -> C.toVal P.<$> hello meta' a'
    HelloWorld'Goodbye a' -> C.toVal P.<$> goodbye meta' a'

-- API Parser
apiParser :: C.ApiParser HelloWorld
apiParser = C.ApiParser
  { hollow = Map.empty
  , struct = Map.fromList
     [ ("Hello", v HelloWorld'Hello)
     , ("Goodbye", v HelloWorld'Goodbye)
     ]
  , enumeration = Map.empty
  , wrap = Map.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- API: HelloWorld
data HelloWorld
  = HelloWorld'Hello Hello
  | HelloWorld'Goodbye Goodbye
  deriving (P.Show, P.Eq)

-- Struct: Hello
data Hello = Hello
  { who :: T.Text
  } deriving (P.Show, P.Eq, P.Generic)

instance A.ToJSON Hello

instance C.ToVal Hello where
  toVal Hello
    { who
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("who", C.toVal who)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> Hello
      P.<$> C.getMember m "who"
    _ -> P.Nothing
