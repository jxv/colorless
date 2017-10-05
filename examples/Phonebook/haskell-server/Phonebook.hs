-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Colorless.Examples.Phonebook
  ( handler'Map
  , handler'PublicSpec
  , Meta'Middlewares(..)
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

import qualified Data.Map as Map
import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor)
import qualified Control.Monad.IO.Class as M (MonadIO)
import Data.Aeson (toJSON, Value)

import qualified Colorless.Examples.Phonebook.V0 as V0
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
    ( M.MonadIO m
    , C.RuntimeThrower m
    , V0.Phonebook'Service meta0 m
    )
  => C.Options
  -> Meta'Middlewares m meta0
  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)
handler'Map options metaMiddlewares = Map.fromList
    [ (0, (0, V0.phonebook'Handler options $ meta'Middleware0 metaMiddlewares))
    ]

handler'PublicSpec :: Value
handler'PublicSpec = toJSON
  [ V0.phonebook'Spec
  ]

