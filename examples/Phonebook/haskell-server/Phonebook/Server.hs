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
module Phonebook.Server
  ( phonebook'handlerMap
  , phonebook'spec
  , phonebook'Scotty'Post
  , phonebook'Scotty'Get
  , V0.PersonId(..)
  , V0.Name(..)
  , V0.Phone(..)
  , V0.Street(..)
  , V0.City(..)
  , V0.Zipcode(..)
  , V0.Address(..)
  , V0.Person(..)
  , V0.LookupPerson(..)
  , V0.LookupPersonByName(..)
  , V0.InsertPerson(..)
  , V0.State(..)
  , V0.State'Other'Members(..)
  , V0.Phonebook'Service(..)
  , V0.Phonebook'Thrower(..)
  , V0.phonebook'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified Fluid.Server.Scotty as Scotty
import qualified Phonebook.Major0 as V0
  ( Phonebook'Service(..)
  , Phonebook'Thrower(..)
  , phonebook'handler
  , phonebook'version
  , phonebook'pull
  , phonebook'spec
  , PersonId(..)
  , Name(..)
  , Phone(..)
  , Street(..)
  , City(..)
  , Zipcode(..)
  , Address(..)
  , Person(..)
  , LookupPerson(..)
  , LookupPersonByName(..)
  , InsertPerson(..)
  , State(..)
  , State'Other'Members(..)
  )

phonebook'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.Phonebook'Service meta0 m
    )
  => (xtra -> C.Hooks m () meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
phonebook'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.phonebook'handler hooks0 xtra))
    ]

phonebook'spec :: R.Value
phonebook'spec = R.toJSON
  [ V0.phonebook'spec
  ]

phonebook'Scotty'Post
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , R.MonadCatch m
    , V0.Phonebook'Service meta0 m
    )
  => C.Pull
  -> ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m () meta0)
  -> Scotty.ScottyT e m ()
phonebook'Scotty'Post pull hooks0 = Scotty.respond pull (phonebook'handlerMap hooks0)

phonebook'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
phonebook'Scotty'Get = Scotty.getSpec phonebook'spec
