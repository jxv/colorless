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
module Phonebook
  ( phonebook'version
  , phonebook'pull
  , phonebook'request
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
  , phonebook'LookupPerson
  , phonebook'LookupPersonByName
  , phonebook'InsertPerson
  , address'Mk
  , person'Mk
  , lookupPerson'Mk
  , lookupPersonByName'Mk
  , insertPerson'Mk
  , personId'Mk
  , name'Mk
  , phone'Mk
  , street'Mk
  , city'Mk
  , zipcode'Mk
  , state'CA'Mk
  , state'NY'Mk
  , state'TX'Mk
  , address'
  , person'
  , lookupPerson'
  , lookupPersonByName'
  , insertPerson'
  , personId'
  , name'
  , phone'
  , street'
  , city'
  , zipcode'
  , state'
  , address'street
  , address'city
  , address'zipcode
  , address'state
  , person'name
  , person'phone
  , person'address
  , person'friends
  , lookupPerson'id
  , lookupPersonByName'name
  , insertPerson'person
  , phonebook'HttpClient'Post
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.String as P (IsString)
import qualified Data.Word as I
import qualified Data.Int as I
import qualified Data.IORef as IO
import qualified Colorless.Client as C
import qualified Colorless.Client.Expr as C
import qualified Colorless.Ast as Ast
import qualified Colorless.Imports as R
import qualified Colorless.Client.HttpClient as HttpClient

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
phonebook'version :: C.Version
phonebook'version = C.Version 0 0

phonebook'pull :: C.Pull
phonebook'pull = C.Pull "http" "127.0.0.1" "/" 8000

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

-- Struct: InsertPerson
data InsertPerson = InsertPerson
  { insertPersonPerson :: Person
  } deriving (P.Show, P.Eq)

-- Enumeration: State
data State
  = State'CA 
  | State'NY
  | State'TX
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- API
--------------------------------------------------------

phonebook'request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => () -> C.Expr a -> C.Request () a
phonebook'request _meta _query = C.Request (C.Version 0 0) phonebook'version _meta _query

phonebook'LookupPerson :: C.Expr LookupPerson -> C.Expr (P.Maybe Person)
phonebook'LookupPerson = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "LookupPerson" P.. Ast.toAst

phonebook'LookupPersonByName :: C.Expr LookupPersonByName -> C.Expr [Person]
phonebook'LookupPersonByName = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "LookupPersonByName" P.. Ast.toAst

phonebook'InsertPerson :: C.Expr InsertPerson -> C.Expr PersonId
phonebook'InsertPerson = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "InsertPerson" P.. Ast.toAst

personId'Mk :: C.Expr (R.Text -> PersonId)
personId'Mk = C.unsafeWrapExpr

personId' :: PersonId -> C.Expr PersonId
personId' = C.unsafeExpr P.. Ast.toAst

name'Mk :: C.Expr (R.Text -> Name)
name'Mk = C.unsafeWrapExpr

name' :: Name -> C.Expr Name
name' = C.unsafeExpr P.. Ast.toAst

phone'Mk :: C.Expr (R.Text -> Phone)
phone'Mk = C.unsafeWrapExpr

phone' :: Phone -> C.Expr Phone
phone' = C.unsafeExpr P.. Ast.toAst

street'Mk :: C.Expr (R.Text -> Street)
street'Mk = C.unsafeWrapExpr

street' :: Street -> C.Expr Street
street' = C.unsafeExpr P.. Ast.toAst

city'Mk :: C.Expr (R.Text -> City)
city'Mk = C.unsafeWrapExpr

city' :: City -> C.Expr City
city' = C.unsafeExpr P.. Ast.toAst

zipcode'Mk :: C.Expr (R.Text -> Zipcode)
zipcode'Mk = C.unsafeWrapExpr

zipcode' :: Zipcode -> C.Expr Zipcode
zipcode' = C.unsafeExpr P.. Ast.toAst

address'Mk :: C.Expr (Street -> City -> Zipcode -> State -> Address)
address'Mk = C.unsafeStructExpr ["street", "city", "zipcode", "state"]

address' :: Address -> C.Expr Address
address' = C.unsafeExpr P.. Ast.toAst

address'street :: C.Path (Address -> Street)
address'street = C.unsafePath ["street"]

address'city :: C.Path (Address -> City)
address'city = C.unsafePath ["city"]

address'zipcode :: C.Path (Address -> Zipcode)
address'zipcode = C.unsafePath ["zipcode"]

address'state :: C.Path (Address -> State)
address'state = C.unsafePath ["state"]

person'Mk :: C.Expr (Name -> Phone -> (P.Maybe Address) -> [PersonId] -> Person)
person'Mk = C.unsafeStructExpr ["name", "phone", "address", "friends"]

person' :: Person -> C.Expr Person
person' = C.unsafeExpr P.. Ast.toAst

person'name :: C.Path (Person -> Name)
person'name = C.unsafePath ["name"]

person'phone :: C.Path (Person -> Phone)
person'phone = C.unsafePath ["phone"]

person'address :: C.Path (Person -> (P.Maybe Address))
person'address = C.unsafePath ["address"]

person'friends :: C.Path (Person -> [PersonId])
person'friends = C.unsafePath ["friends"]

lookupPerson'Mk :: C.Expr (PersonId -> LookupPerson)
lookupPerson'Mk = C.unsafeStructExpr ["id"]

lookupPerson' :: LookupPerson -> C.Expr LookupPerson
lookupPerson' = C.unsafeExpr P.. Ast.toAst

lookupPerson'id :: C.Path (LookupPerson -> PersonId)
lookupPerson'id = C.unsafePath ["id"]

lookupPersonByName'Mk :: C.Expr (Name -> LookupPersonByName)
lookupPersonByName'Mk = C.unsafeStructExpr ["name"]

lookupPersonByName' :: LookupPersonByName -> C.Expr LookupPersonByName
lookupPersonByName' = C.unsafeExpr P.. Ast.toAst

lookupPersonByName'name :: C.Path (LookupPersonByName -> Name)
lookupPersonByName'name = C.unsafePath ["name"]

insertPerson'Mk :: C.Expr (Person -> InsertPerson)
insertPerson'Mk = C.unsafeStructExpr ["person"]

insertPerson' :: InsertPerson -> C.Expr InsertPerson
insertPerson' = C.unsafeExpr P.. Ast.toAst

insertPerson'person :: C.Path (InsertPerson -> Person)
insertPerson'person = C.unsafePath ["person"]

state'CA'Mk :: C.Expr State
state'CA'Mk = C.unsafeExpr P.. Ast.toAst P.$ State'CA

state'NY'Mk :: C.Expr State
state'NY'Mk = C.unsafeExpr P.. Ast.toAst P.$ State'NY

state'TX'Mk :: C.Expr State
state'TX'Mk = C.unsafeExpr P.. Ast.toAst P.$ State'TX

state' :: State -> C.Expr State
state' = C.unsafeExpr P.. Ast.toAst

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

phonebook'HttpClient'Post
  :: (C.HasType a, Ast.ToAst a, R.FromJSON a)
  => HttpClient.Manager
  -> C.Pull
  -> HttpClient.RequestHeaders
  -> C.Request () a
  -> P.IO (HttpClient.HttpClientResponse R.ByteString, P.Maybe (C.Response () a))
phonebook'HttpClient'Post = HttpClient.sendRequest

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.HasType PersonId where
  getType _ = "PersonId"

instance C.ToVal PersonId where
  toVal (PersonId _w) = C.toVal _w

instance C.FromVal PersonId where
  fromVal _v = PersonId P.<$> C.fromVal _v

instance C.ToExpr PersonId

instance R.ToJSON PersonId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON PersonId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst PersonId where
  toAst (PersonId _w) = Ast.toAst _w

instance C.HasType Name where
  getType _ = "Name"

instance C.ToVal Name where
  toVal (Name _w) = C.toVal _w

instance C.FromVal Name where
  fromVal _v = Name P.<$> C.fromVal _v

instance C.ToExpr Name

instance R.ToJSON Name where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Name where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Name where
  toAst (Name _w) = Ast.toAst _w

instance C.HasType Phone where
  getType _ = "Phone"

instance C.ToVal Phone where
  toVal (Phone _w) = C.toVal _w

instance C.FromVal Phone where
  fromVal _v = Phone P.<$> C.fromVal _v

instance C.ToExpr Phone

instance R.ToJSON Phone where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Phone where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Phone where
  toAst (Phone _w) = Ast.toAst _w

instance C.HasType Street where
  getType _ = "Street"

instance C.ToVal Street where
  toVal (Street _w) = C.toVal _w

instance C.FromVal Street where
  fromVal _v = Street P.<$> C.fromVal _v

instance C.ToExpr Street

instance R.ToJSON Street where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Street where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Street where
  toAst (Street _w) = Ast.toAst _w

instance C.HasType City where
  getType _ = "City"

instance C.ToVal City where
  toVal (City _w) = C.toVal _w

instance C.FromVal City where
  fromVal _v = City P.<$> C.fromVal _v

instance C.ToExpr City

instance R.ToJSON City where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON City where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst City where
  toAst (City _w) = Ast.toAst _w

instance C.HasType Zipcode where
  getType _ = "Zipcode"

instance C.ToVal Zipcode where
  toVal (Zipcode _w) = C.toVal _w

instance C.FromVal Zipcode where
  fromVal _v = Zipcode P.<$> C.fromVal _v

instance C.ToExpr Zipcode

instance R.ToJSON Zipcode where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Zipcode where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Zipcode where
  toAst (Zipcode _w) = Ast.toAst _w

instance C.HasType Address where
  getType _ = "Address"

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

instance C.ToExpr Address

instance R.ToJSON Address where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Address where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Address where
  toAst Address
    { addressStreet
    , addressCity
    , addressZipcode
    , addressState
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("street", Ast.toAst addressStreet)
    , ("city", Ast.toAst addressCity)
    , ("zipcode", Ast.toAst addressZipcode)
    , ("state", Ast.toAst addressState)
    ]

instance C.HasType Person where
  getType _ = "Person"

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

instance C.ToExpr Person

instance R.ToJSON Person where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Person where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst Person where
  toAst Person
    { personName
    , personPhone
    , personAddress
    , personFriends
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("name", Ast.toAst personName)
    , ("phone", Ast.toAst personPhone)
    , ("address", Ast.toAst personAddress)
    , ("friends", Ast.toAst personFriends)
    ]

instance C.HasType LookupPerson where
  getType _ = "LookupPerson"

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

instance C.ToExpr LookupPerson

instance R.ToJSON LookupPerson where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON LookupPerson where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst LookupPerson where
  toAst LookupPerson
    { lookupPersonId
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("id", Ast.toAst lookupPersonId)
    ]

instance C.HasType LookupPersonByName where
  getType _ = "LookupPersonByName"

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

instance C.ToExpr LookupPersonByName

instance R.ToJSON LookupPersonByName where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON LookupPersonByName where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst LookupPersonByName where
  toAst LookupPersonByName
    { lookupPersonByNameName
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("name", Ast.toAst lookupPersonByNameName)
    ]

instance C.HasType InsertPerson where
  getType _ = "InsertPerson"

instance C.ToVal InsertPerson where
  toVal InsertPerson
    { insertPersonPerson
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("person", C.toVal insertPersonPerson)
    ]

instance C.FromVal InsertPerson where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> InsertPerson
      P.<$> C.getMember _m "person"
    _ -> P.Nothing

instance C.ToExpr InsertPerson

instance R.ToJSON InsertPerson where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON InsertPerson where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst InsertPerson where
  toAst InsertPerson
    { insertPersonPerson
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("person", Ast.toAst insertPersonPerson)
    ]

instance C.HasType State where
  getType _ = "State"

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

instance C.ToExpr State

instance R.ToJSON State where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON State where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance Ast.ToAst State where
  toAst = \case
    State'CA -> Ast.Ast'Enumeral P.$ Ast.Enumeral "CA" P.Nothing
    State'NY -> Ast.Ast'Enumeral P.$ Ast.Enumeral "NY" P.Nothing
    State'TX -> Ast.Ast'Enumeral P.$ Ast.Enumeral "TX" P.Nothing

