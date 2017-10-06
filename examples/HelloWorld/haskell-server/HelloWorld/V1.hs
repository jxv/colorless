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
module Colorless.Examples.HelloWorld.V1
  ( helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Handler
  , helloWorld'Spec
  , HelloWorld'Thrower(..)
  , HelloWorld'Service(..)
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)

import qualified Colorless.Imports as R
import qualified Colorless.Server as C

import Colorless.Examples.HelloWorld.V0 (Hello(..))

-- Version
helloWorld'Version :: C.Version
helloWorld'Version = C.Version 1 0

helloWorld'Pull :: C.Pull
helloWorld'Pull = C.Pull "http" "127.0.0.1" "/" 8080

-- Thrower
class P.Monad m => HelloWorld'Thrower m where
  helloWorld'Throw :: () -> m a

-- Service
class HelloWorld'Thrower m => HelloWorld'Service meta m where
  hello :: meta -> Hello -> m R.Text
  goodbye :: meta -> Goodbye -> m ()

-- Handler
helloWorld'Handler :: (HelloWorld'Service meta m, C.RuntimeThrower m, R.MonadIO m) => C.Options -> (() -> m meta) -> C.Request -> m C.Response
helloWorld'Handler options metaMiddleware C.Request{meta,query} = do
  meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
  xformMeta <- metaMiddleware meta'
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
helloWorld'ApiCall :: (HelloWorld'Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
helloWorld'ApiCall meta' apiCall' = case C.parseApiCall helloWorld'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow C.RuntimeError'UnrecognizedCall
  P.Just x' -> case x' of
    HelloWorld'Api'Hello a' -> C.toVal P.<$> hello meta' a'
    HelloWorld'Api'Goodbye a' -> C.toVal P.<$> goodbye meta' a'

-- API Parser
helloWorld'ApiParser :: C.ApiParser HelloWorld'Api
helloWorld'ApiParser = C.ApiParser
  { hollow = R.empty
  , struct = R.fromList
     [ ("Hello", v HelloWorld'Api'Hello)
     , ("Goodbye", v HelloWorld'Api'Goodbye)
     ]
  , enumeration = R.empty
  , wrap = R.empty
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
  { target :: R.Text
  } deriving (P.Show, P.Eq)

instance C.ToVal Goodbye where
  toVal Goodbye
    { target
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("target", C.toVal target)
    ]

instance C.FromVal Goodbye where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Goodbye
      P.<$> C.getMember _m "target"
    _ -> P.Nothing

instance R.ToJSON Goodbye where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Goodbye where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

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
  } deriving (P.Show, P.Eq)

instance C.ToVal Color where
  toVal = \case
    Color'Red -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Red" P.Nothing
    Color'Green -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Green" P.Nothing
    Color'Blue -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Blue" P.Nothing
    Color'Custom Color'Custom'Members
      { r
      , g
      , b
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Custom" P.$ P.Just P.$ R.fromList
      [ ("r", C.toVal r)
      , ("g", C.toVal g)
      , ("b", C.toVal b)
      ]
    Color'Yellow -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Yellow" P.Nothing

instance C.FromVal Color where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Red", P.Nothing) -> P.Just Color'Red
      ("Green", P.Nothing) -> P.Just Color'Green
      ("Blue", P.Nothing) -> P.Just Color'Blue
      ("Custom", P.Just _m') -> Color'Custom P.<$> (Color'Custom'Members
          P.<$> C.getMember _m' "r"
          P.<*> C.getMember _m' "g"
          P.<*> C.getMember _m' "b"
        )
      ("Yellow", P.Nothing) -> P.Just Color'Yellow
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Color where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Color where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

helloWorld'Spec :: R.Value
helloWorld'Spec = v
  where P.Just v = R.decode "{\"colorless\":{\"major\":0,\"minor\":0},\"types\":[{\"n\":\"Hello\",\"m\":[{\"target\":\"String\"}],\"o\":\"String\"},{\"n\":\"Goodbye\",\"m\":[{\"target\":\"String\"}],\"o\":\"Unit\"},{\"n\":\"Color\",\"e\":[{\"tag\":\"Red\"},{\"tag\":\"Green\"},{\"tag\":\"Blue\"},{\"tag\":\"Custom\",\"m\":[{\"r\":\"U8\"},{\"g\":\"U8\"},{\"b\":\"U8\"}]},{\"tag\":\"Yellow\"}]}],\"pull\":{\"protocol\":\"http\",\"name\":\"HelloWorld\",\"address\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/\",\"port\":8080,\"error\":\"Unit\"},\"version\":{\"major\":1,\"minor\":0}}"

