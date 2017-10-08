-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module HelloWorld
  ( handler'Map
  , handler'PublicSpec
  , Meta'Middlewares(..)
  , helloWorld'Scotty'SendResponse
  , helloWorld'Scotty'GetSpec
  , V0.HelloWorld'Service(..)
  , V0.HelloWorld'Thrower(..)
  , V0.helloWorld'Pull
  , V0.Hello(..)
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

data Meta'Middlewares m meta0
  = Meta'Middlewares
  { meta'Middleware0 :: () -> m meta0
  }

handler'Map
  ::
    ( R.MonadIO m
    , V0.HelloWorld'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
handler'Map options metaMiddlewares = R.fromList
    [ (0, (0, V0.helloWorld'Handler options P.$ meta'Middleware0 metaMiddlewares))
    ]

handler'PublicSpec :: R.Value
handler'PublicSpec = R.toJSON
  [ V0.helloWorld'Spec
  ]

helloWorld'Scotty'SendResponse
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , V0.HelloWorld'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> C.Pull
  -> Scotty.ScottyT e m ()
helloWorld'Scotty'SendResponse options metaMiddlewares pull = Scotty.sendResponse pull (handler'Map options metaMiddlewares)

helloWorld'Scotty'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
helloWorld'Scotty'GetSpec = Scotty.getSpec handler'PublicSpec

