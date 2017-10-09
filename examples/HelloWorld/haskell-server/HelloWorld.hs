-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module HelloWorld
  ( helloWorld'handlerMap
  , helloWorld'spec
  , Meta'Middlewares(..)
  , helloWorld'Scotty'Post
  , helloWorld'Scotty'Get
  , V0.HelloWorld'Service(..)
  , V0.HelloWorld'Thrower(..)
  , V0.helloWorld'pull
  , V0.Hello(..)
  ) where

import qualified Prelude as P
import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor, Pull)
import qualified Colorless.Imports as R
import qualified Colorless.Server.Scotty as Scotty

import qualified HelloWorld.V0 as V0
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'handler
  , helloWorld'version
  , helloWorld'pull
  , helloWorld'spec
  , Hello(..)
  )

data Meta'Middlewares m meta0
  = Meta'Middlewares
  { meta'Middleware0 :: () -> m meta0
  }

helloWorld'handlerMap
  ::
    ( R.MonadIO m
    , V0.HelloWorld'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
helloWorld'handlerMap options metaMiddlewares = R.fromList
    [ (0, (0, V0.helloWorld'Handler options P.$ meta'Middleware0 metaMiddlewares))
    ]

helloWorld'spec :: R.Value
helloWorld'spec = R.toJSON
  [ V0.helloWorld'spec
  ]

helloWorld'Scotty'Post
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , V0.HelloWorld'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> C.Pull
  -> Scotty.ScottyT e m ()
helloWorld'Scotty'Post options metaMiddlewares pull = Scotty.sendResponse pull (helloWorld'handlerMap options metaMiddlewares)

helloWorld'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
helloWorld'Scotty'Get = Scotty.getSpec helloWorld'spec

