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
module HelloWorld
  ( helloWorld'handlerMap
  , helloWorld'spec
  , helloWorld'Scotty'Post
  , helloWorld'Scotty'Get
  , V0.Hello(..)
  , V0.HelloWorld'Service(..)
  , V0.HelloWorld'Thrower(..)
  , V0.helloWorld'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified Fluid.Server.Scotty as Scotty
import qualified HelloWorld.V0 as V0
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

helloWorld'Scotty'Post
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , R.MonadCatch m
    , V0.HelloWorld'Service meta0 m
    )
  => C.Pull
  -> ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m () meta0)
  -> Scotty.ScottyT e m ()
helloWorld'Scotty'Post pull hooks0 = Scotty.respond pull (helloWorld'handlerMap hooks0)

helloWorld'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
helloWorld'Scotty'Get = Scotty.getSpec helloWorld'spec
