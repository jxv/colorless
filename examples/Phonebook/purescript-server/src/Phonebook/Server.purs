
-- Module
module Phonebook.Server
  ( phonebook'handlerMap
  , phonebook'spec
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

import Prelude as P
import Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import Fluid.Imports as R
import Phonebook.Major0 as V0
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
