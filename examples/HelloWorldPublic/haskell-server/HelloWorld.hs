-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module HelloWorld
  ( handler'Map
  , handler'PublicSpec
  , Meta'Middlewares(..)
  , helloWorld'Scotty'SendResponse
  , helloWorld'Scotty'GetSpec
  , V2.HelloWorld'Service(..)
  , V2.HelloWorld'Thrower(..)
  , V2.helloWorld'Pull
  , V2.Hello(..)
  , V2.Goodbye(..)
  , V2.Color(..)
  , V2.Color'Custom'Members(..)
  ) where

import qualified Prelude as P
import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor, Pull)
import qualified Colorless.Imports as R
import qualified Colorless.Server.Scotty as Scotty

import qualified HelloWorld.V0 as V0
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'Handler
  , helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Spec
  , Hello(..)
  )

import qualified HelloWorld.V1 as V1
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'Handler
  , helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Spec
  , Hello(..)
  , Goodbye(..)
  , Color(..)
  , Color'Custom'Members(..)
  )

import qualified HelloWorld.V2 as V2
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'Handler
  , helloWorld'Version
  , helloWorld'Pull
  , helloWorld'Spec
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
    ( R.MonadIO m
    , V0.HelloWorld'Service meta0 m
    , V1.HelloWorld'Service meta1 m
    , V2.HelloWorld'Service meta2 m
    )
  => C.Options
  -> Meta'Middlewares m meta0 meta1 meta2
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
handler'Map options metaMiddlewares = R.fromList
    [ (0, (1, V0.helloWorld'Handler options P.$ meta'Middleware0 metaMiddlewares))
    , (1, (0, V1.helloWorld'Handler options P.$ meta'Middleware1 metaMiddlewares))
    , (2, (0, V2.helloWorld'Handler options P.$ meta'Middleware2 metaMiddlewares))
    ]

handler'PublicSpec :: R.Value
handler'PublicSpec = R.toJSON
  [ V0.helloWorld'Spec
  , V1.helloWorld'Spec
  , V2.helloWorld'Spec
  ]

helloWorld'Scotty'SendResponse
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , V0.HelloWorld'Service meta0 m
    , V1.HelloWorld'Service meta1 m
    , V2.HelloWorld'Service meta2 m
    )
  => C.Options
  -> Meta'Middlewares m meta0 meta1 meta2
  -> C.Pull
  -> Scotty.ScottyT e m ()
helloWorld'Scotty'SendResponse options metaMiddlewares pull = Scotty.sendResponse pull (handler'Map options metaMiddlewares)

helloWorld'Scotty'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
helloWorld'Scotty'GetSpec = Scotty.getSpec handler'PublicSpec

