-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
module Colorless.Examples.Phonebook.V0
  ( phonebook'Version
  , phonebook'Handler
  , Phonebook'Thrower(..)
  , Phonebook'Service(..)
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
  ) where

-- Imports
import qualified Prelude as P
import qualified Data.Map as Map
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Conversions as T
import qualified Data.String as P (IsString)
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified GHC.Generics as P (Generic)
import qualified Colorless.Server as C


-- Version
phonebook'Version :: C.Version
phonebook'Version = C.Version 0 0

-- Thrower
class P.Monad m => Phonebook'Thrower m where
  phonebook'Throw :: () -> m a

-- Service
class Phonebook'Thrower m => Phonebook'Service meta m where
  lookupPerson :: meta -> LookupPerson -> m (P.Maybe Person)
  lookupPersonByName :: meta -> LookupPersonByName -> m [Person]

-- Handler
phonebook'Handler :: (Phonebook'Service meta m, C.RuntimeThrower m, IO.MonadIO m) => C.Options -> (() -> m meta) -> C.Request -> m C.Response
phonebook'Handler options metaMiddleware C.Request{meta,query} = do
  meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
  xformMeta <- metaMiddleware meta'
  envRef <- IO.liftIO C.emptyEnv
  variableBaseCount <- IO.liftIO (Map.size P.<$> IO.readIORef envRef)
  let options' = C.Options
        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)
        }
  let evalConfig = C.EvalConfig
        { C.options = options'
        , C.apiCall = phonebook'ApiCall xformMeta
        }
  query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
  vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
  P.return (C.Response'Success (A.toJSON vals))

-- API
phonebook'ApiCall :: (Phonebook'Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
phonebook'ApiCall meta' apiCall' = case C.parseApiCall phonebook'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow C.RuntimeError'UnrecognizedCall
  P.Just x' -> case x' of
    Phonebook'Api'LookupPerson a' -> C.toVal P.<$> lookupPerson meta' a'
    Phonebook'Api'LookupPersonByName a' -> C.toVal P.<$> lookupPersonByName meta' a'

-- API Parser
phonebook'ApiParser :: C.ApiParser Phonebook'Api
phonebook'ApiParser = C.ApiParser
  { hollow = Map.empty
  , struct = Map.fromList
     [ ("LookupPerson", v Phonebook'Api'LookupPerson)
     , ("LookupPersonByName", v Phonebook'Api'LookupPersonByName)
     ]
  , enumeration = Map.empty
  , wrap = Map.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data Phonebook'Api
  = Phonebook'Api'LookupPerson LookupPerson
  | Phonebook'Api'LookupPersonByName LookupPersonByName
  deriving (P.Show, P.Eq)

-- Wrap: PersonId
newtype PersonId = PersonId T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType PersonId where
  getType _ = "PersonId"

-- Wrap: Name
newtype Name = Name T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Name where
  getType _ = "Name"

-- Wrap: Phone
newtype Phone = Phone T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Phone where
  getType _ = "Phone"

-- Wrap: Street
newtype Street = Street T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Street where
  getType _ = "Street"

-- Wrap: City
newtype City = City T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType City where
  getType _ = "City"

-- Wrap: Zipcode
newtype Zipcode = Zipcode T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Zipcode where
  getType _ = "Zipcode"

-- Struct: Address
data Address = Address
  { street :: Street
  , city :: City
  , zipcode :: Zipcode
  , state :: State
  } deriving (P.Show, P.Eq, P.Generic)

instance C.HasType Address where
  getType _ = "Address"

instance A.ToJSON Address

instance C.ToVal Address where
  toVal Address
    { street
    , city
    , zipcode
    , state
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("street", C.toVal street)
    , ("city", C.toVal city)
    , ("zipcode", C.toVal zipcode)
    , ("state", C.toVal state)
    ]

instance C.FromVal Address where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> Address
      P.<$> C.getMember m "street"
      P.<*> C.getMember m "city"
      P.<*> C.getMember m "zipcode"
      P.<*> C.getMember m "state"
    _ -> P.Nothing

-- Struct: Person
data Person = Person
  { name :: Name
  , phone :: Phone
  , address :: (P.Maybe Address)
  , friends :: [PersonId]
  } deriving (P.Show, P.Eq, P.Generic)

instance C.HasType Person where
  getType _ = "Person"

instance A.ToJSON Person

instance C.ToVal Person where
  toVal Person
    { name
    , phone
    , address
    , friends
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("name", C.toVal name)
    , ("phone", C.toVal phone)
    , ("address", C.toVal address)
    , ("friends", C.toVal friends)
    ]

instance C.FromVal Person where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> Person
      P.<$> C.getMember m "name"
      P.<*> C.getMember m "phone"
      P.<*> C.getMember m "address"
      P.<*> C.getMember m "friends"
    _ -> P.Nothing

-- Struct: LookupPerson
data LookupPerson = LookupPerson
  { id :: PersonId
  } deriving (P.Show, P.Eq, P.Generic)

instance C.HasType LookupPerson where
  getType _ = "LookupPerson"

instance A.ToJSON LookupPerson

instance C.ToVal LookupPerson where
  toVal LookupPerson
    { id
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("id", C.toVal id)
    ]

instance C.FromVal LookupPerson where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> LookupPerson
      P.<$> C.getMember m "id"
    _ -> P.Nothing

-- Struct: LookupPersonByName
data LookupPersonByName = LookupPersonByName
  { name :: T.Text
  } deriving (P.Show, P.Eq, P.Generic)

instance C.HasType LookupPersonByName where
  getType _ = "LookupPersonByName"

instance A.ToJSON LookupPersonByName

instance C.ToVal LookupPersonByName where
  toVal LookupPersonByName
    { name
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ Map.fromList
    [ ("name", C.toVal name)
    ]

instance C.FromVal LookupPersonByName where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct m)) -> LookupPersonByName
      P.<$> C.getMember m "name"
    _ -> P.Nothing

-- Enumeration: State
data State
  = State'CA 
  | State'NY
  | State'TX
  deriving (P.Show, P.Eq)

instance C.HasType State where
  getType _ = "State"

instance A.ToJSON State where
  toJSON = \case
    State'CA -> A.object [ "tag" A..= ("CA" :: T.Text) ]
    State'NY -> A.object [ "tag" A..= ("NY" :: T.Text) ]
    State'TX -> A.object [ "tag" A..= ("TX" :: T.Text) ]

instance C.FromVal State where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral tag m)) -> case (tag,m) of
      ("CA", P.Nothing) -> P.Just State'CA
      ("NY", P.Nothing) -> P.Just State'NY
      ("TX", P.Nothing) -> P.Just State'TX
      _ -> P.Nothing
    _ -> P.Nothing

instance C.ToVal State where
  toVal = \case
    State'CA -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "CA" P.Nothing
    State'NY -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "NY" P.Nothing
    State'TX -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "TX" P.Nothing

