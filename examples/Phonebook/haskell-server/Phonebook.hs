-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Phonebook
  ( phonebook'handlerMap
  , phonebook'spec
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
import qualified Colorless.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
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

phonebook'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.Phonebook'Service meta0 m
    )
  => C.Hooks m () meta0

  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
phonebook'handlerMap hooks0 = R.fromList
    [ (0, (0, V0.phonebook'handler hooks0))
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
  -> C.Hooks m () meta0
  -> Scotty.ScottyT e m ()
phonebook'Scotty'Post pull hooks0 = Scotty.sendResponse pull (phonebook'handlerMap hooks0)

phonebook'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
phonebook'Scotty'Get = Scotty.getSpec phonebook'spec

