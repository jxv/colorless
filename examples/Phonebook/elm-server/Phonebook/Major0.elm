
--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Struct: Address
type Address =
  { street : Street
  , city : City
  , zipcode : Zipcode
  , state : State
  }

-- Struct: Person
type Person =
  { name : Name
  , homeNumber : Phone
  , cellNumber : Phone
  , address : (Maybe Address)
  , friends : (List PersonId)
  }

-- Struct: LookupPerson
type LookupPerson =
  { id : PersonId
  }

-- Struct: LookupPersonByName
type LookupPersonByName =
  { name : Name
  }

-- Struct: InsertPerson
type InsertPerson =
  { person : Person
  }
