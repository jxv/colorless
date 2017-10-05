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
module Colorless.Examples.Phonebook
  ( phonebook'Version
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
import qualified Colorless.Client as C
import qualified Colorless.Client.Expr as C
import qualified Colorless.Ast as Ast

-- Version
phonebook'Version :: C.Version
phonebook'Version = C.Version 0 0

lookupPerson :: C.Expr LookupPerson -> C.Expr (P.Maybe Person)
lookupPerson expr'' = C.unsafeExpr (Ast.Ast'StructCall (Ast.StructCall "LookupPerson" (Ast.toAst expr'')))

lookupPersonByName :: C.Expr LookupPersonByName -> C.Expr [Person]
lookupPersonByName expr'' = C.unsafeExpr (Ast.Ast'StructCall (Ast.StructCall "LookupPersonByName" (Ast.toAst expr'')))

-- Wrap: PersonId
newtype PersonId = PersonId T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType PersonId where
  getType _ = "PersonId"

instance Ast.ToAst PersonId where
  toAst (PersonId w) = Ast.toAst w

personId'Mk :: C.Expr (T.Text -> PersonId)
personId'Mk = C.unsafeWrapExpr

personId' :: PersonId -> C.Expr PersonId
personId' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Name
newtype Name = Name T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Name where
  getType _ = "Name"

instance Ast.ToAst Name where
  toAst (Name w) = Ast.toAst w

name'Mk :: C.Expr (T.Text -> Name)
name'Mk = C.unsafeWrapExpr

name' :: Name -> C.Expr Name
name' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Phone
newtype Phone = Phone T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Phone where
  getType _ = "Phone"

instance Ast.ToAst Phone where
  toAst (Phone w) = Ast.toAst w

phone'Mk :: C.Expr (T.Text -> Phone)
phone'Mk = C.unsafeWrapExpr

phone' :: Phone -> C.Expr Phone
phone' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Street
newtype Street = Street T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Street where
  getType _ = "Street"

instance Ast.ToAst Street where
  toAst (Street w) = Ast.toAst w

street'Mk :: C.Expr (T.Text -> Street)
street'Mk = C.unsafeWrapExpr

street' :: Street -> C.Expr Street
street' = C.unsafeExpr P.. Ast.toAst

-- Wrap: City
newtype City = City T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType City where
  getType _ = "City"

instance Ast.ToAst City where
  toAst (City w) = Ast.toAst w

city'Mk :: C.Expr (T.Text -> City)
city'Mk = C.unsafeWrapExpr

city' :: City -> C.Expr City
city' = C.unsafeExpr P.. Ast.toAst

-- Wrap: Zipcode
newtype Zipcode = Zipcode T.Text
  deriving (P.Show, P.Eq, P.Ord, P.IsString, T.ToText, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)

instance C.HasType Zipcode where
  getType _ = "Zipcode"

instance Ast.ToAst Zipcode where
  toAst (Zipcode w) = Ast.toAst w

zipcode'Mk :: C.Expr (T.Text -> Zipcode)
zipcode'Mk = C.unsafeWrapExpr

zipcode' :: Zipcode -> C.Expr Zipcode
zipcode' = C.unsafeExpr P.. Ast.toAst

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

instance Ast.ToAst Address where
  toAst Address
    { street
    , city
    , zipcode
    , state
    } = Ast.Ast'Struct P.. Ast.Struct P.$ Map.fromList
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

instance Ast.ToAst Person where
  toAst Person
    { name
    , phone
    , address
    , friends
    } = Ast.Ast'Struct P.. Ast.Struct P.$ Map.fromList
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

instance Ast.ToAst LookupPerson where
  toAst LookupPerson
    { id
    } = Ast.Ast'Struct P.. Ast.Struct P.$ Map.fromList
    [ ("id", Ast.toAst id)
    ]

lookupPerson'Mk :: C.Expr (PersonId -> LookupPerson)
lookupPerson'Mk = C.unsafeStructExpr ["id"]

lookupPerson' :: LookupPerson -> C.Expr LookupPerson
lookupPerson' = C.unsafeExpr P.. Ast.toAst

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

instance Ast.ToAst LookupPersonByName where
  toAst LookupPersonByName
    { name
    } = Ast.Ast'Struct P.. Ast.Struct P.$ Map.fromList
    [ ("name", Ast.toAst name)
    ]

lookupPersonByName'Mk :: C.Expr (T.Text -> LookupPersonByName)
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

