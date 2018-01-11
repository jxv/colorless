
--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Struct: Address
record Address where
    constructor MkAddress
    street : Street
    city : City
    zipcode : Zipcode
    state : State

-- Struct: Person
record Person where
    constructor MkPerson
    name : Name
    phone : Phone
    address : (Maybe Address)
    friends : (List PersonId)

-- Struct: LookupPerson
record LookupPerson where
    constructor MkLookupPerson
    id : PersonId

-- Struct: LookupPersonByName
record LookupPersonByName where
    constructor MkLookupPersonByName
    name : Name

-- Struct: InsertPerson
record InsertPerson where
    constructor MkInsertPerson
    person : Person
