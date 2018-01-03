-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
module HelloWorld.Client
  ( helloWorld'version
  , helloWorld'pull
  , helloWorld'request
  , Hello(..)
  , helloWorld'HttpClient'Post
  , helloWorld'Hello
  , hello'Mk
  , hello'
  , hello'target
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.String as P (IsString)
import qualified Data.IORef as IO
import qualified Fluid.Client as C
import qualified Fluid.Client.Expr as C
import qualified Fluid.Ast as Ast
import qualified Fluid.Imports as R
import qualified Fluid.Client.HttpClient as HttpClient

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
helloWorld'version :: C.Version
helloWorld'version = C.Version 0 0

helloWorld'pull :: C.Pull
helloWorld'pull = C.Pull "http" "127.0.0.1" "/" 8080

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Struct: Hello
data Hello = Hello
  { helloTarget :: R.Text
  } deriving (P.Show, P.Eq)

--------------------------------------------------------
-- API
--------------------------------------------------------

helloWorld'request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => () -> C.Expr a -> C.Request () a
helloWorld'request _meta _query = C.Request (C.Version 0 0) helloWorld'version _meta _query

helloWorld'Hello :: C.Expr Hello -> C.Expr R.Text
helloWorld'Hello = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "Hello" P.. Ast.toAst

hello'Mk :: C.Expr (R.Text -> Hello)
hello'Mk = C.unsafeStructExpr ["target"]

hello' :: Hello -> C.Expr Hello
hello' = C.unsafeExpr P.. Ast.toAst

hello'target :: C.Path (Hello -> R.Text)
hello'target = C.unsafePath ["target"]

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

helloWorld'HttpClient'Post
  :: (C.HasType a, Ast.ToAst a, C.FromVal a)
  => HttpClient.Manager
  -> C.Pull
  -> HttpClient.RequestHeaders
  -> C.Request () a
  -> P.IO (HttpClient.HttpClientResponse R.ByteString, P.Maybe (C.Response () a))
helloWorld'HttpClient'Post = HttpClient.sendRequest

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.HasType Hello where
  getType _ = "Hello"

instance C.ToVal Hello where
  toVal Hello
    { helloTarget
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("target", C.toVal helloTarget)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Hello
      P.<$> C.getMember _m "target"
    _ -> P.Nothing

instance C.ToExpr Hello

instance R.ToJSON Hello where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Hello where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Hello where
  toAst Hello
    { helloTarget
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("target", Ast.toAst helloTarget)
    ]
