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
module Colorless.Examples.HelloWorld.V1
  ( helloWorld'Version
  , helloWorld'Handler
  , HelloWorld'Thrower(..)
  , HelloWorld'Service(..)
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
import qualified Colorless.Server as C

import Colorless.Examples.HelloWorld.V0 (Hello(..))

-- Version
helloWorld'Version :: C.Version
helloWorld'Version = C.Version 1 0

-- Thrower
class P.Monad m => HelloWorld'Thrower m where
  helloWorld'Throw :: () -> m a

-- Service
class HelloWorld'Thrower m => HelloWorld'Service meta m where
  hello :: meta -> Hello -> m T.Text
  goodbye :: meta -> Goodbye -> m ()

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
    HelloWorld'Api'Hello a' -> C.toVal P.<$> hello meta' a'
    HelloWorld'Api'Goodbye a' -> C.toVal P.<$> goodbye meta' a'

-- API Parser
helloWorld'ApiParser :: C.ApiParser HelloWorld'Api
helloWorld'ApiParser = C.ApiParser
  { hollow = Map.empty
  , struct = Map.fromList
     [ ("Hello", v HelloWorld'Api'Hello)
     , ("Goodbye", v HelloWorld'Api'Goodbye)
     ]
  , enumeration = Map.empty
  , wrap = Map.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data HelloWorld'Api
  = HelloWorld'Api'Hello Hello
  | HelloWorld'Api'Goodbye Goodbye
  deriving (P.Show, P.Eq)

-- Struct: Goodbye
data Goodbye = Goodbye
  { target :: T.Text
  } deriving (P.Show, P.Eq, P.Generic)

instance A.ToJSON Goodbye

instance C.ToVal Goodbye where
  toVal Goodbye
    { target
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("target", C.toVal target)
    ]

instance C.FromVal Goodbye where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> Goodbye
      P.<$> C.getMember m "target"
    _ -> P.Nothing

-- Enumeration: Color
data Color
  = Color'Red 
  | Color'Green
  | Color'Blue
  | Color'Custom Color'Custom'Members
  | Color'Yellow
  deriving (P.Show, P.Eq)

data Color'Custom'Members = Color'Custom'Members
  { r :: I.Word8
  , g :: I.Word8
  , b :: I.Word8
  } deriving (P.Show, P.Eq, P.Generic)

instance A.ToJSON Color'Custom'Members

instance A.ToJSON Color where
  toJSON = \case
    Color'Red -> A.object [ "tag" A..= ("Red" :: T.Text) ]
    Color'Green -> A.object [ "tag" A..= ("Green" :: T.Text) ]
    Color'Blue -> A.object [ "tag" A..= ("Blue" :: T.Text) ]
    Color'Custom m -> C.combineObjects (A.object [ "tag" A..= ("Custom" :: T.Text) ]) (A.toJSON m)
    Color'Yellow -> A.object [ "tag" A..= ("Yellow" :: T.Text) ]

instance C.FromVal Color where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral tag m)) -> case (tag,m) of
      ("Red", P.Nothing) -> P.Just Color'Red
      ("Green", P.Nothing) -> P.Just Color'Green
      ("Blue", P.Nothing) -> P.Just Color'Blue
      ("Custom", P.Just m') -> Color'Custom P.<$> (Color'Custom'Members
          P.<$> C.getMember m' "r"
          P.<*> C.getMember m' "g"
          P.<*> C.getMember m' "b"
        )
      ("Yellow", P.Nothing) -> P.Just Color'Yellow
      _ -> P.Nothing
    _ -> P.Nothing

instance C.ToVal Color where
  toVal = \case
    Color'Red -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Red" P.Nothing
    Color'Green -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Green" P.Nothing
    Color'Blue -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Blue" P.Nothing
    Color'Custom Color'Custom'Members
      { r
      , g
      , b
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Custom" P.$ P.Just P.$ Map.fromList
      [ ("r", C.toVal r)
      , ("g", C.toVal g)
      , ("b", C.toVal b)
      ]
    Color'Yellow -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Yellow" P.Nothing

