-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Colorless.Examples.HelloWorld
  ( handler'Map
  , Meta'Middlewares(..)
  , V2.HelloWorld'Service(..)
  , V2.HelloWorld'Thrower(..)
  , V2.helloWorld'Pull
  , V2.Hello(..)
  , V2.Goodbye(..)
  , V2.Color(..)
  , V2.Color'Custom'Members(..)
  ) where

import qualified Data.Map as Map
import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor)
import qualified Control.Monad.IO.Class as M (MonadIO)

import qualified Colorless.Examples.HelloWorld.V0 as V0
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'Handler
  , helloWorld'Version
  , helloWorld'Pull
  , Hello(..)
  )

import qualified Colorless.Examples.HelloWorld.V1 as V1
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'Handler
  , helloWorld'Version
  , helloWorld'Pull
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  )

import qualified Colorless.Examples.HelloWorld.V2 as V2
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'Handler
  , helloWorld'Version
  , helloWorld'Pull
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  )

data Meta'Middlewares m meta0 meta1 meta2
  = Meta'Middlewares
  { meta'Middleware0 :: () -> m meta0
  , meta'Middleware1 :: () -> m meta1
  , meta'Middleware2 :: () -> m meta2
  }

handler'Map
  ::
    ( M.MonadIO m
    , C.RuntimeThrower m
    , V0.HelloWorld'Service meta0 m
    , V1.HelloWorld'Service meta1 m
    , V2.HelloWorld'Service meta2 m
    )
  => C.Options
  -> Meta'Middlewares m meta0 meta1 meta2
  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)
handler'Map options metaMiddlewares = Map.fromList
    [ (0, (1, V0.helloWorld'Handler options $ meta'Middleware0 metaMiddlewares))
    , (1, (0, V1.helloWorld'Handler options $ meta'Middleware1 metaMiddlewares))
    , (2, (0, V2.helloWorld'Handler options $ meta'Middleware2 metaMiddlewares))
    ]

