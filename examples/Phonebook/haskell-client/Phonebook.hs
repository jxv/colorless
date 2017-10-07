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
  ( phonebook'Version
  , phonebook'Pull
  , phonebook'Request
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
  , lookupPerson
  , lookupPersonByName
  , address'Mk
  , person'Mk
  , lookupPerson'Mk
  , lookupPersonByName'Mk
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
  , phonebook'HttpClient'SendRequest
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

-- Version
phonebook'Version :: C.Version
phonebook'Version = C.Version 0 0

phonebook'Pull :: C.Pull
phonebook'Pull = C.Pull "http" "127.0.0.1" "/" 8000

phonebook'Request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => () -> C.Expr a -> C.Request () a
phonebook'Request _meta _query = C.Request (C.Version 0 0) phonebook'Version _meta _query

lookupPerson :: C.Expr LookupPerson -> C.Expr (P.Maybe Person)
lookupPerson = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "LookupPerson" P.. Ast.toAst

lookupPersonByName :: C.Expr LookupPersonByName -> C.Expr [Person]
lookupPersonByName = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall "LookupPersonByName" P.. Ast.toAst

phonebook'HttpClient'SendRequest
  :: (C.HasType a, Ast.ToAst a, R.FromJSON a)
  => HttpClient.Manager
  -> C.Pull
  -> HttpClient.RequestHeaders
  -> C.Request () a
  -> P.IO (HttpClient.HttpClientResponse R.ByteString, P.Maybe (C.Response () a))
phonebook'HttpClient'SendRequest = HttpClient.sendRequest

-- Wrap: PersonId
newtype PersonId = PersonId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

instance C.HasType PersonId where
  getType _ = "PersonId"

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

instance Ast.ToAst PersonId where
  toAst (PersonId w) = Ast.toAst w

personId'Mk :: C.Expr (R.Text -> PersonId)
personId'Mk = C.unsafeWrapExpr

personId' :: PersonId -> C.Expr PersonId
personId' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Name
newtype Name = Name R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

instance C.HasType Name where
  getType _ = "Name"

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

instance Ast.ToAst Name where
  toAst (Name w) = Ast.toAst w

name'Mk :: C.Expr (R.Text -> Name)
name'Mk = C.unsafeWrapExpr

name' :: Name -> C.Expr Name
name' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Phone
newtype Phone = Phone R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

instance C.HasType Phone where
  getType _ = "Phone"

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

instance Ast.ToAst Phone where
  toAst (Phone w) = Ast.toAst w

phone'Mk :: C.Expr (R.Text -> Phone)
phone'Mk = C.unsafeWrapExpr

phone' :: Phone -> C.Expr Phone
phone' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Street
newtype Street = Street R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

instance C.HasType Street where
  getType _ = "Street"

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

instance Ast.ToAst Street where
  toAst (Street w) = Ast.toAst w

street'Mk :: C.Expr (R.Text -> Street)
street'Mk = C.unsafeWrapExpr

street' :: Street -> C.Expr Street
street' = C.unsafeExpr P.. Ast.toAst

-- Wrap: City
newtype City = City R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

instance C.HasType City where
  getType _ = "City"

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

instance Ast.ToAst City where
  toAst (City w) = Ast.toAst w

city'Mk :: C.Expr (R.Text -> City)
city'Mk = C.unsafeWrapExpr

city' :: City -> C.Expr City
city' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Zipcode
newtype Zipcode = Zipcode R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

instance C.HasType Zipcode where
  getType _ = "Zipcode"

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

instance Ast.ToAst Zipcode where
  toAst (Zipcode w) = Ast.toAst w

zipcode'Mk :: C.Expr (R.Text -> Zipcode)
zipcode'Mk = C.unsafeWrapExpr

zipcode' :: Zipcode -> C.Expr Zipcode
zipcode' = C.unsafeExpr P.. Ast.toAst

-- Struct: Address
data Address = Address
  { street :: Street
  , city :: City
  , zipcode :: Zipcode
  , state :: State
  } deriving (P.Show, P.Eq)

instance C.HasType Address where
  getType _ = "Address"

instance C.ToVal Address where
  toVal Address
    { street
    , city
    , zipcode
    , state
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("street", C.toVal street)
    , ("city", C.toVal city)
    , ("zipcode", C.toVal zipcode)
    , ("state", C.toVal state)
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

address'street :: C.Path (Address -> Street)
address'street = C.unsafePath ["street"]

address'city :: C.Path (Address -> City)
address'city = C.unsafePath ["city"]

address'zipcode :: C.Path (Address -> Zipcode)
address'zipcode = C.unsafePath ["zipcode"]

address'state :: C.Path (Address -> State)
address'state = C.unsafePath ["state"]

instance Ast.ToAst Address where
  toAst Address
    { street
    , city
    , zipcode
    , state
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("street", Ast.toAst street)
    , ("city", Ast.toAst city)
    , ("zipcode", Ast.toAst zipcode)
    , ("state", Ast.toAst state)
    ]

address'Mk :: C.Expr (Street -> City -> Zipcode -> State -> Address)
address'Mk = C.unsafeStructExpr ["street", "city", "zipcode", "state"]

address' :: Address -> C.Expr Address
address' = C.unsafeExpr P.. Ast.toAst

-- Struct: Person
data Person = Person
  { name :: Name
  , phone :: Phone
  , address :: (P.Maybe Address)
  , friends :: [PersonId]
  } deriving (P.Show, P.Eq)

instance C.HasType Person where
  getType _ = "Person"

instance C.ToVal Person where
  toVal Person
    { name
    , phone
    , address
    , friends
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal name)
    , ("phone", C.toVal phone)
    , ("address", C.toVal address)
    , ("friends", C.toVal friends)
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

person'name :: C.Path (Person -> Name)
person'name = C.unsafePath ["name"]

person'phone :: C.Path (Person -> Phone)
person'phone = C.unsafePath ["phone"]

person'address :: C.Path (Person -> (P.Maybe Address))
person'address = C.unsafePath ["address"]

person'friends :: C.Path (Person -> [PersonId])
person'friends = C.unsafePath ["friends"]

instance Ast.ToAst Person where
  toAst Person
    { name
    , phone
    , address
    , friends
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("name", Ast.toAst name)
    , ("phone", Ast.toAst phone)
    , ("address", Ast.toAst address)
    , ("friends", Ast.toAst friends)
    ]

person'Mk :: C.Expr (Name -> Phone -> (P.Maybe Address) -> [PersonId] -> Person)
person'Mk = C.unsafeStructExpr ["name", "phone", "address", "friends"]

person' :: Person -> C.Expr Person
person' = C.unsafeExpr P.. Ast.toAst

-- Struct: LookupPerson
data LookupPerson = LookupPerson
  { id :: PersonId
  } deriving (P.Show, P.Eq)

instance C.HasType LookupPerson where
  getType _ = "LookupPerson"

instance C.ToVal LookupPerson where
  toVal LookupPerson
    { id
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("id", C.toVal id)
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

lookupPerson'id :: C.Path (LookupPerson -> PersonId)
lookupPerson'id = C.unsafePath ["id"]

instance Ast.ToAst LookupPerson where
  toAst LookupPerson
    { id
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("id", Ast.toAst id)
    ]

lookupPerson'Mk :: C.Expr (PersonId -> LookupPerson)
lookupPerson'Mk = C.unsafeStructExpr ["id"]

lookupPerson' :: LookupPerson -> C.Expr LookupPerson
lookupPerson' = C.unsafeExpr P.. Ast.toAst

-- Struct: LookupPersonByName
data LookupPersonByName = LookupPersonByName
  { name :: R.Text
  } deriving (P.Show, P.Eq)

instance C.HasType LookupPersonByName where
  getType _ = "LookupPersonByName"

instance C.ToVal LookupPersonByName where
  toVal LookupPersonByName
    { name
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal name)
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

lookupPersonByName'name :: C.Path (LookupPersonByName -> R.Text)
lookupPersonByName'name = C.unsafePath ["name"]

instance Ast.ToAst LookupPersonByName where
  toAst LookupPersonByName
    { name
    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList
    [ ("name", Ast.toAst name)
    ]

lookupPersonByName'Mk :: C.Expr (R.Text -> LookupPersonByName)
lookupPersonByName'Mk = C.unsafeStructExpr ["name"]

lookupPersonByName' :: LookupPersonByName -> C.Expr LookupPersonByName
lookupPersonByName' = C.unsafeExpr P.. Ast.toAst

-- Enumeration: State
data State
  = State'CA 
  | State'NY
  | State'TX
  deriving (P.Show, P.Eq)

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

state'CA'Mk :: C.Expr State
state'CA'Mk = C.unsafeExpr P.. Ast.toAst P.$ State'CA

state'NY'Mk :: C.Expr State
state'NY'Mk = C.unsafeExpr P.. Ast.toAst P.$ State'NY

state'TX'Mk :: C.Expr State
state'TX'Mk = C.unsafeExpr P.. Ast.toAst P.$ State'TX

state' :: State -> C.Expr State
state' = C.unsafeExpr P.. Ast.toAst

