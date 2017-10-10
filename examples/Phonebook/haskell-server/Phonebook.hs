-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Phonebook
  ( phonebook'handlerMap
  , phonebook'spec
  , Meta'Middlewares(..)
  , phonebook'Scotty'Post
  , phonebook'Scotty'Get
  , V0.Phonebook'Service(..)
  , V0.Phonebook'Thrower(..)
  , V0.phonebook'pull
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
  ) where

import qualified Prelude as P
import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor, Pull)
import qualified Colorless.Imports as R
import qualified Colorless.Server.Scotty as Scotty

import qualified Phonebook.V0 as V0
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
  )

data Meta'Middlewares m meta0
  = Meta'Middlewares
  { meta'Middleware0 :: () -> m meta0
  }

phonebook'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.Phonebook'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
phonebook'handlerMap options metaMiddlewares = R.fromList
    [ (0, (0, V0.phonebook'handler options P.$ meta'Middleware0 metaMiddlewares))
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
  => C.Options
  -> Meta'Middlewares m meta0
  -> C.Pull
  -> Scotty.ScottyT e m ()
phonebook'Scotty'Post options metaMiddlewares pull = Scotty.sendResponse pull (phonebook'handlerMap options metaMiddlewares)

phonebook'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
phonebook'Scotty'Get = Scotty.getSpec phonebook'spec

