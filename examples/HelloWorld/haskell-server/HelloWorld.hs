-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Colorless.Examples.HelloWorld
  ( mkHandleRequestMap
  , MetaMiddlewares(..)
  , V2.Service(..)
  , V2.ServiceThrower(..)
  , V2.Hello(..)
  , V2.Goodbye(..)
  , V2.Color(..)
  , V2.Color'Custom'Members(..)
  ) where

import qualified Data.Map as Map
import qualified Colorless.Types as C (RuntimeThrower, Options, Request, Response, Major, Minor)
import qualified Control.Monad.IO.Class as M (MonadIO)

import qualified Colorless.Examples.HelloWorld.V0 as V0
  ( Service(..)
  , ServiceThrower(..)
  , handleRequest
  , version
  , Hello(..)
  )

import qualified Colorless.Examples.HelloWorld.V1 as V1
  ( Service(..)
  , ServiceThrower(..)
  , handleRequest
  , version
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  )

import qualified Colorless.Examples.HelloWorld.V2 as V2
  ( Service(..)
  , ServiceThrower(..)
  , handleRequest
  , version
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  )

data MetaMiddlewares m meta0 meta1 meta2
  = MetaMiddlewares
  { metaMiddleware0 :: () -> m meta0
  , metaMiddleware1 :: () -> m meta1
  , metaMiddleware2 :: () -> m meta2
  }

mkHandleRequestMap
  ::
    ( M.MonadIO m
    , C.RuntimeThrower m
    , V0.Service meta0 m
    , V1.Service meta1 m
    , V2.Service meta2 m
    )
  => C.Options
  -> MetaMiddlewares m meta0 meta1 meta2
  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)
mkHandleRequestMap options metaMiddlewares = Map.fromList
    [ (0, (1, V0.handleRequest options $ metaMiddleware0 metaMiddlewares))
    , (1, (0, V1.handleRequest options $ metaMiddleware1 metaMiddlewares))
    , (2, (0, V2.handleRequest options $ metaMiddleware2 metaMiddlewares))
    ]

