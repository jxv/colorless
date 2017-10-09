-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
module Phonebook.V0
  ( phonebook'Version
  , phonebook'Pull
  , phonebook'Handler
  , phonebook'Spec
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
  , phonebook'Scotty'SendResponse
  , phonebook'Scotty'GetSpec
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Except as M
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)

import qualified Colorless.Imports as R
import qualified Colorless.Server as C


import qualified Colorless.Server.Scotty as Scotty

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
phonebook'Version :: C.Version
phonebook'Version = C.Version 0 0

phonebook'Pull :: C.Pull
phonebook'Pull = C.Pull "http" "127.0.0.1" "/" 8000

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

-- Thrower
class C.ServiceThrower m => Phonebook'Thrower m where
  phonebook'Throw :: () -> m a
  phonebook'Throw = C.serviceThrow P.. R.toJSON

-- Service
class P.Monad m => Phonebook'Service meta m where
  phonebook'lookupPerson :: meta -> LookupPerson -> m (P.Maybe Person)
  phonebook'lookupPersonByName :: meta -> LookupPersonByName -> m [Person]

instance Phonebook'Service meta m => Phonebook'Service meta (M.ExceptT C.Response m) where
  phonebook'lookupPerson _meta = M.lift  P.. phonebook'lookupPerson _meta
  phonebook'lookupPersonByName _meta = M.lift  P.. phonebook'lookupPersonByName _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: PersonId
newtype PersonId = PersonId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: Name
newtype Name = Name R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: Phone
newtype Phone = Phone R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: Street
newtype Street = Street R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: City
newtype City = City R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: Zipcode
newtype Zipcode = Zipcode R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Struct: Address
data Address = Address
  { addressStreet :: Street
  , addressCity :: City
  , addressZipcode :: Zipcode
  , addressState :: State
  } deriving (P.Show, P.Eq)

-- Struct: Person
data Person = Person
  { personName :: Name
  , personPhone :: Phone
  , personAddress :: (P.Maybe Address)
  , personFriends :: [PersonId]
  } deriving (P.Show, P.Eq)

-- Struct: LookupPerson
data LookupPerson = LookupPerson
  { lookupPersonId :: PersonId
  } deriving (P.Show, P.Eq)

-- Struct: LookupPersonByName
data LookupPersonByName = LookupPersonByName
  { lookupPersonByNameName :: Name
  } deriving (P.Show, P.Eq)

-- Enumeration: State
data State
  = State'CA 
  | State'NY
  | State'TX
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

phonebook'Scotty'SendResponse
  :: (Scotty.ScottyError e, R.MonadIO m, Phonebook'Service meta m)
  => C.Options
  -> (() -> m meta)
  -> C.Pull
  -> Scotty.ScottyT e m ()
phonebook'Scotty'SendResponse _options _metaMiddleware _pull = Scotty.sendResponseSingleton _pull phonebook'Version (phonebook'Handler _options _metaMiddleware)

phonebook'Scotty'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
phonebook'Scotty'GetSpec = Scotty.getSpec P.$ R.toJSON [phonebook'Spec]

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

-- Handler
phonebook'Handler
  :: (Phonebook'Service meta m, R.MonadIO m)
  => C.Options
  -> (() -> m meta)
  -> C.Request
  -> m (P.Either C.Response C.Response)
phonebook'Handler options metaMiddleware C.Request{meta,query} = M.runExceptT P.$ do
  meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
  xformMeta <- M.lift P.$ metaMiddleware meta'
  envRef <- R.liftIO C.emptyEnv
  variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)
  let options' = C.Options
        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)
        }
  let evalConfig = C.EvalConfig
        { C.options = options'
        , C.apiCall = phonebook'ApiCall xformMeta
        }
  query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
  vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
  P.return (C.Response'Success (R.toJSON vals))

-- API
phonebook'ApiCall :: (Phonebook'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
phonebook'ApiCall meta' apiCall' = case C.parseApiCall phonebook'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow C.RuntimeError'UnrecognizedCall
  P.Just x' -> case x' of
    Phonebook'Api'LookupPerson a' -> C.toVal P.<$> phonebook'lookupPerson meta' a'
    Phonebook'Api'LookupPersonByName a' -> C.toVal P.<$> phonebook'lookupPersonByName meta' a'

-- API Parser
phonebook'ApiParser :: C.ApiParser Phonebook'Api
phonebook'ApiParser = C.ApiParser
  { hollow = R.empty
  , struct = R.fromList
     [ ("LookupPerson", v Phonebook'Api'LookupPerson)
     , ("LookupPersonByName", v Phonebook'Api'LookupPersonByName)
     ]
  , enumeration = R.empty
  , wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data Phonebook'Api
  = Phonebook'Api'LookupPerson LookupPerson
  | Phonebook'Api'LookupPersonByName LookupPersonByName
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal PersonId where
  toVal (PersonId _w) = C.toVal _w

instance C.FromVal PersonId where
  fromVal _v = PersonId P.<$> C.fromVal _v

instance R.ToJSON PersonId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON PersonId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Name where
  toVal (Name _w) = C.toVal _w

instance C.FromVal Name where
  fromVal _v = Name P.<$> C.fromVal _v

instance R.ToJSON Name where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Name where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Phone where
  toVal (Phone _w) = C.toVal _w

instance C.FromVal Phone where
  fromVal _v = Phone P.<$> C.fromVal _v

instance R.ToJSON Phone where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Phone where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Street where
  toVal (Street _w) = C.toVal _w

instance C.FromVal Street where
  fromVal _v = Street P.<$> C.fromVal _v

instance R.ToJSON Street where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Street where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal City where
  toVal (City _w) = C.toVal _w

instance C.FromVal City where
  fromVal _v = City P.<$> C.fromVal _v

instance R.ToJSON City where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON City where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Zipcode where
  toVal (Zipcode _w) = C.toVal _w

instance C.FromVal Zipcode where
  fromVal _v = Zipcode P.<$> C.fromVal _v

instance R.ToJSON Zipcode where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Zipcode where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Address where
  toVal Address
    { addressStreet
    , addressCity
    , addressZipcode
    , addressState
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("street", C.toVal addressStreet)
    , ("city", C.toVal addressCity)
    , ("zipcode", C.toVal addressZipcode)
    , ("state", C.toVal addressState)
    ]

instance C.FromVal Address where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Address
      P.<$> C.getMember _m "street"
      P.<*> C.getMember _m "city"
      P.<*> C.getMember _m "zipcode"
      P.<*> C.getMember _m "state"
    _ -> P.Nothing

instance R.ToJSON Address where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Address where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Person where
  toVal Person
    { personName
    , personPhone
    , personAddress
    , personFriends
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal personName)
    , ("phone", C.toVal personPhone)
    , ("address", C.toVal personAddress)
    , ("friends", C.toVal personFriends)
    ]

instance C.FromVal Person where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Person
      P.<$> C.getMember _m "name"
      P.<*> C.getMember _m "phone"
      P.<*> C.getMember _m "address"
      P.<*> C.getMember _m "friends"
    _ -> P.Nothing

instance R.ToJSON Person where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Person where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal LookupPerson where
  toVal LookupPerson
    { lookupPersonId
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("id", C.toVal lookupPersonId)
    ]

instance C.FromVal LookupPerson where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> LookupPerson
      P.<$> C.getMember _m "id"
    _ -> P.Nothing

instance R.ToJSON LookupPerson where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON LookupPerson where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal LookupPersonByName where
  toVal LookupPersonByName
    { lookupPersonByNameName
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal lookupPersonByNameName)
    ]

instance C.FromVal LookupPersonByName where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> LookupPersonByName
      P.<$> C.getMember _m "name"
    _ -> P.Nothing

instance R.ToJSON LookupPersonByName where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON LookupPersonByName where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal State where
  toVal = \case
    State'CA -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "CA" P.Nothing
    State'NY -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "NY" P.Nothing
    State'TX -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "TX" P.Nothing

instance C.FromVal State where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("CA", P.Nothing) -> P.Just State'CA
      ("NY", P.Nothing) -> P.Just State'NY
      ("TX", P.Nothing) -> P.Just State'TX
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON State where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON State where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

phonebook'Spec :: R.Value
phonebook'Spec = v
  where P.Just v = R.decode "{\"colorless\":{\"major\":0,\"minor\":0},\"types\":[{\"n\":\"PersonId\",\"w\":\"String\"},{\"n\":\"Name\",\"w\":\"String\"},{\"n\":\"Phone\",\"w\":\"String\"},{\"n\":\"Street\",\"w\":\"String\"},{\"n\":\"City\",\"w\":\"String\"},{\"n\":\"State\",\"e\":[{\"tag\":\"CA\"},{\"tag\":\"NY\"},{\"tag\":\"TX\"}]},{\"n\":\"Zipcode\",\"w\":\"String\"},{\"n\":\"Address\",\"m\":[{\"street\":\"Street\"},{\"city\":\"City\"},{\"zipcode\":\"Zipcode\"},{\"state\":\"State\"}]},{\"n\":\"Person\",\"m\":[{\"name\":\"Name\"},{\"phone\":\"Phone\"},{\"address\":{\"n\":\"Option\",\"p\":\"Address\"}},{\"friends\":{\"n\":\"List\",\"p\":\"PersonId\"}}]},{\"n\":\"LookupPerson\",\"m\":[{\"id\":\"PersonId\"}],\"o\":{\"n\":\"Option\",\"p\":\"Person\"}},{\"n\":\"LookupPersonByName\",\"m\":[{\"name\":\"Name\"}],\"o\":{\"n\":\"List\",\"p\":\"Person\"}}],\"pull\":{\"protocol\":\"http\",\"name\":\"Phonebook\",\"host\":\"127.0.0.1\",\"path\":\"/\",\"port\":8000,\"error\":\"Unit\",\"meta\":\"Unit\"},\"version\":{\"major\":0,\"minor\":0}}"

