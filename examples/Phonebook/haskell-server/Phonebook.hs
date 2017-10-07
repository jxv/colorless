-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Phonebook
  ( handler'Map
  , handler'PublicSpec
  , Meta'Middlewares(..)
  , phonebook'Scotty'SendResponse
  , phonebook'Scotty'GetSpec
  , V0.Phonebook'Service(..)
  , V0.Phonebook'Thrower(..)
  , V0.phonebook'Pull
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
  , V0.State(..)
  ) where

import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor)
import qualified Colorless.Imports as R
import qualified Colorless.Server.Scotty as Scotty

import qualified Phonebook.V0 as V0
  ( Phonebook'Service(..)
  , Phonebook'Thrower(..)
  , phonebook'Handler
  , phonebook'Version
  , phonebook'Pull
  , phonebook'Spec
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
  , State(..)
  )

data Meta'Middlewares m meta0
  = Meta'Middlewares
  { meta'Middleware0 :: () -> m meta0
  }

handler'Map
  ::
    ( R.MonadIO m
    , C.RuntimeThrower m
    , V0.Phonebook'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> R.Map C.Major (C.Minor, C.Request -> m C.Response)
handler'Map options metaMiddlewares = R.fromList
    [ (0, (0, V0.phonebook'Handler options $ meta'Middleware0 metaMiddlewares))
    ]

handler'PublicSpec :: R.Value
handler'PublicSpec = R.toJSON
  [ V0.phonebook'Spec
  ]

phonebook'Scotty'SendResponse
  :: (Scotty.ScottyError e, R.MonadIO m, C.RuntimeThrower m, Phonebook'Service meta m)
  -> C.Options
  -> Meta'Middlewares m meta0
  -> C.Pull
  -> Scotty.ScottyT e m ()
phonebook'Scotty'SendResponse options metaMiddlewares pull = ScottyT.sendResponse pull phonebook'Version (handler'Map options metaMiddlewares)

phonebook'Scotty'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
phonebook'Scotty'GetSpec = ScottyT.getSpec phonebook'PublicSpec

