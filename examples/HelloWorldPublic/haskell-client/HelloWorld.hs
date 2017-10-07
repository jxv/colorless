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
module Colorless.Examples.HelloWorld
  ( helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Request
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  , hello
  , goodbye
  , hello'Mk
  , goodbye'Mk
  , color'Red'Mk
  , color'Blue'Mk
  , color'Green'Mk
  , color'Yellow'Mk
  , color'Custom'Mk
  , hello'
  , goodbye'
  , color'
  , hello'who
  , goodbye'target
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
helloWorld'Version = C.Version 2 0

helloWorld'Pull :: C.Pull
helloWorld'Pull = C.Pull "http" "127.0.0.1" "/" 8080

helloWorld'Request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => () -> C.Expr a -> C.Request () a
helloWorld'Request _meta _query = C.Request (C.Version 0 0) helloWorld'Version _meta _query

hello :: C.Expr Hello -> C.Expr R.Text
hello = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "Hello" P.. Ast.toAst

goodbye :: C.Expr Goodbye -> C.Expr ()
goodbye = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "Goodbye" P.. Ast.toAst

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
  { who :: R.Text
  } deriving (P.Show, P.Eq)

instance C.HasType Hello where
  getType _ = "Hello"

instance C.ToVal Hello where
  toVal Hello
    { who
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("who", C.toVal who)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Hello
      P.<$> C.getMember _m "who"
    _ -> P.Nothing

instance R.ToJSON Hello where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Hello where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

hello'who :: C.Path (Hello -> R.Text)
hello'who = C.unsafePath ["who"]

instance Ast.ToAst Hello where
  toAst Hello
    { who
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("who", Ast.toAst who)
    ]

hello'Mk :: C.Expr (R.Text -> Hello)
hello'Mk = C.unsafeStructExpr ["who"]

hello' :: Hello -> C.Expr Hello
hello' = C.unsafeExpr P.. Ast.toAst

-- Struct: Goodbye
data Goodbye = Goodbye
  { target :: R.Text
  } deriving (P.Show, P.Eq)

instance C.HasType Goodbye where
  getType _ = "Goodbye"

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

goodbye'target :: C.Path (Goodbye -> R.Text)
goodbye'target = C.unsafePath ["target"]

instance Ast.ToAst Goodbye where
  toAst Goodbye
    { target
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("target", Ast.toAst target)
    ]

goodbye'Mk :: C.Expr (R.Text -> Goodbye)
goodbye'Mk = C.unsafeStructExpr ["target"]

goodbye' :: Goodbye -> C.Expr Goodbye
goodbye' = C.unsafeExpr P.. Ast.toAst

-- Enumeration: Color
data Color
  = Color'Red 
  | Color'Blue
  | Color'Green
  | Color'Yellow
  | Color'Custom Color'Custom'Members
  deriving (P.Show, P.Eq)

data Color'Custom'Members = Color'Custom'Members
  { r :: I.Word8
  , g :: I.Word8
  , b :: I.Word8
  } deriving (P.Show, P.Eq)

instance C.HasType Color where
  getType _ = "Color"

instance C.ToVal Color where
  toVal = \case
    Color'Red -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Red" P.Nothing
    Color'Blue -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Blue" P.Nothing
    Color'Green -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Green" P.Nothing
    Color'Yellow -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Yellow" P.Nothing
    Color'Custom Color'Custom'Members
      { r
      , g
      , b
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Custom" P.$ P.Just P.$ R.fromList
      [ ("r", C.toVal r)
      , ("g", C.toVal g)
      , ("b", C.toVal b)
      ]

instance C.FromVal Color where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Red", P.Nothing) -> P.Just Color'Red
      ("Blue", P.Nothing) -> P.Just Color'Blue
      ("Green", P.Nothing) -> P.Just Color'Green
      ("Yellow", P.Nothing) -> P.Just Color'Yellow
      ("Custom", P.Just _m') -> Color'Custom P.<$> (Color'Custom'Members
          P.<$> C.getMember _m' "r"
          P.<*> C.getMember _m' "g"
          P.<*> C.getMember _m' "b"
        )
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

instance Ast.ToAst Color where
  toAst = \case
    Color'Red -> Ast.Ast'Enumeral P.$ Ast.Enumeral "Red" P.Nothing
    Color'Blue -> Ast.Ast'Enumeral P.$ Ast.Enumeral "Blue" P.Nothing
    Color'Green -> Ast.Ast'Enumeral P.$ Ast.Enumeral "Green" P.Nothing
    Color'Yellow -> Ast.Ast'Enumeral P.$ Ast.Enumeral "Yellow" P.Nothing
    Color'Custom Color'Custom'Members
      { r
      , g
      , b
      } -> Ast.Ast'Enumeral P.$ Ast.Enumeral "Custom" P.$ P.Just P.$ R.fromList
      [ ("r", Ast.toAst r)
      , ("g", Ast.toAst g)
      , ("b", Ast.toAst b)
      ]

color'Red'Mk :: C.Expr Color
color'Red'Mk = C.unsafeExpr P.. Ast.toAst P.$ Color'Red

color'Blue'Mk :: C.Expr Color
color'Blue'Mk = C.unsafeExpr P.. Ast.toAst P.$ Color'Blue

color'Green'Mk :: C.Expr Color
color'Green'Mk = C.unsafeExpr P.. Ast.toAst P.$ Color'Green

color'Yellow'Mk :: C.Expr Color
color'Yellow'Mk = C.unsafeExpr P.. Ast.toAst P.$ Color'Yellow

color'Custom'Mk :: C.Expr (I.Word8 -> I.Word8 -> I.Word8 -> Color)
color'Custom'Mk = C.unsafeEnumeralExpr "Custom" ["r", "g", "b"]

color' :: Color -> C.Expr Color
color' = C.unsafeExpr P.. Ast.toAst

