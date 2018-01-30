-- Module
module Phonebook.Major0
  ( phonebook'version
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
  ) where

-- Imports
import Data.Maybe as Maybe
import Data.Either as Either

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
phonebook'version :: { major :: Int, minor :: Int }
phonebook'version = { major: 0, minor: 0 }

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: PersonId
newtype PersonId = PersonId String

-- Wrap: Name
newtype Name = Name String

-- Wrap: Phone
newtype Phone = Phone String

-- Wrap: Street
newtype Street = Street String

-- Wrap: City
newtype City = City String

-- Wrap: Zipcode
newtype Zipcode = Zipcode String

-- Struct: Address
data Address = Address
  { street :: Street
  , city :: City
  , zipcode :: Zipcode
  , state :: State
  }

-- Struct: Person
data Person = Person
  { name :: Name
  , homeNumber :: Phone
  , cellNumber :: Phone
  , address :: (Maybe.Maybe Address)
  , friends :: [PersonId]
  }

-- Struct: LookupPerson
data LookupPerson = LookupPerson
  { id :: PersonId
  }

-- Struct: LookupPersonByName
data LookupPersonByName = LookupPersonByName
  { name :: Name
  }

-- Struct: InsertPerson
data InsertPerson = InsertPerson
  { person :: Person
  }

-- Enumeration: State
data State
  = State'CA
  | State'NY
  | State'TX
  | State'Other State'Other'Members

data State'Other'Members = State'Other'Members
  { state'OtherName :: String
  }

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

--------------------------------------------------------
-- Spec
--------------------------------------------------------
