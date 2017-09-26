-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module Colorless.Examples.Phonebook
  ( mkHandleRequestMap
  , MetaMiddlewares(..)
  , V0.Service(..)
  , V0.ServiceThrower(..)
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
import qualified Colorless.Types as C (RuntimeThrower, Options, Request, Response, Major, Minor)
import qualified Control.Monad.IO.Class as M (MonadIO)

import qualified Colorless.Examples.Phonebook.V0 as V0
  ( Service(..)
  , ServiceThrower(..)
  , handleRequest
  , version
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

data MetaMiddlewares m meta0
  = MetaMiddlewares
  { metaMiddleware0 :: () -> m meta0
  }

mkHandleRequestMap
  ::
    ( M.MonadIO m
    , C.RuntimeThrower m
    , V0.Service meta0 m
    )
  => C.Options
  -> MetaMiddlewares m meta0
  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)
mkHandleRequestMap options metaMiddlewares = Map.fromList
    [ (0, (0, V0.handleRequest options $ metaMiddleware0 metaMiddlewares))
    ]

