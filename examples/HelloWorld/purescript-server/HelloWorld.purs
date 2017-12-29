
-- Module
module HelloWorld
  ( helloWorld'handlerMap
  , helloWorld'spec
  , V0.Hello(..)
  , V0.HelloWorld'Service(..)
  , V0.HelloWorld'Thrower(..)
  , V0.helloWorld'pull
  ) where

import Prelude as P
import Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import Fluid.Imports as R
import HelloWorld.V0 as V0
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'handler
  , helloWorld'version
  , helloWorld'pull
  , helloWorld'spec
  , Hello(..)
  )

helloWorld'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.HelloWorld'Service meta0 m
    )
  => (xtra -> C.Hooks m () meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
helloWorld'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.helloWorld'handler hooks0 xtra))
    ]

helloWorld'spec :: R.Value
helloWorld'spec = R.toJSON
  [ V0.helloWorld'spec
  ]
