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
module Colorless.Examples.HelloWorld
  ( helloWorld'Version
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  , hello'Call
  , goodbye'Call
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
import qualified Colorless.Client as C
import qualified Colorless.Client.Expr as C
import qualified Colorless.Ast as Ast

-- Version
helloWorld'Version :: C.Version
helloWorld'Version = C.Version 2 0

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
  | Color'Blue
  | Color'Green
  | Color'Yellow
  | Color'Custom Color'Custom'Members
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
    Color'Blue -> A.object [ "tag" A..= ("Blue" :: T.Text) ]
    Color'Green -> A.object [ "tag" A..= ("Green" :: T.Text) ]
    Color'Yellow -> A.object [ "tag" A..= ("Yellow" :: T.Text) ]
    Color'Custom m -> C.combineObjects (A.object [ "tag" A..= ("Custom" :: T.Text) ]) (A.toJSON m)

instance C.FromVal Color where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral tag m)) -> case (tag,m) of
      ("Red", P.Nothing) -> P.Just Color'Red
      ("Blue", P.Nothing) -> P.Just Color'Blue
      ("Green", P.Nothing) -> P.Just Color'Green
      ("Yellow", P.Nothing) -> P.Just Color'Yellow
      ("Custom", P.Just m') -> Color'Custom P.<$> (Color'Custom'Members
          P.<$> C.getMember m' "r"
          P.<*> C.getMember m' "g"
          P.<*> C.getMember m' "b"
        )
      _ -> P.Nothing
    _ -> P.Nothing

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
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Custom" P.$ P.Just P.$ Map.fromList
      [ ("r", C.toVal r)
      , ("g", C.toVal g)
      , ("b", C.toVal b)
      ]

hello'Call :: C.Expr Hello -> C.Expr T.Text
hello'Call expr'' = C.unsafeExpr (Ast.Ast'StructCall (Ast.StructCall "Hello" (Ast.toAst expr'')))

goodbye'Call :: C.Expr Goodbye -> C.Expr ()
goodbye'Call expr'' = C.unsafeExpr (Ast.Ast'StructCall (Ast.StructCall "Goodbye" (Ast.toAst expr'')))

