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
module HelloWorld
  ( helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Request
  , Hello(..)
  , hello
  , hello'Mk
  , hello'
  , hello'target
  , helloWorld'HttpClient'SendRequest
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.String as P (IsString)
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified Colorless.Client as C
import qualified Colorless.Client.Expr as C
import qualified Colorless.Ast as Ast
import qualified Colorless.Imports as R
import qualified Colorless.Client.HttpClient as HttpClient

-- Version
helloWorld'Version :: C.Version
helloWorld'Version = C.Version 0 0

helloWorld'Pull :: C.Pull
helloWorld'Pull = C.Pull "http" "127.0.0.1" "/" 8080

helloWorld'Request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => () -> C.Expr a -> C.Request () a
helloWorld'Request _meta _query = C.Request (C.Version 0 0) helloWorld'Version _meta _query

hello :: C.Expr Hello -> C.Expr R.Text
hello = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "Hello" P.. Ast.toAst

helloWorld'HttpClient'SendRequest
  :: (C.HasType a, Ast.ToAst a, R.FromJSON a)
  => HttpClient.Manager
  -> C.Pull
  -> HttpClient.RequestHeaders
  -> C.Request () a
  -> P.IO (HttpClient.HttpClientResponse R.ByteString, P.Maybe (C.Response () a))
helloWorld'HttpClient'SendRequest = HttpClient.sendRequest

-- Struct: Hello
data Hello = Hello
  { target :: R.Text
  } deriving (P.Show, P.Eq)

instance C.HasType Hello where
  getType _ = "Hello"

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

hello'target :: C.Path (Hello -> R.Text)
hello'target = C.unsafePath ["target"]

instance Ast.ToAst Hello where
  toAst Hello
    { target
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("target", Ast.toAst target)
    ]

hello'Mk :: C.Expr (R.Text -> Hello)
hello'Mk = C.unsafeStructExpr ["target"]

hello' :: Hello -> C.Expr Hello
hello' = C.unsafeExpr P.. Ast.toAst

