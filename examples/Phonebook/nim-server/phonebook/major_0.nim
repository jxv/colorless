////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

type
# Struct: Address
  Address = object
    street: Street
    city: City
    zipcode: Zipcode
    state: State

type
# Struct: Person
  Person = object
    name: Name
    homeNumber: Phone
    cellNumber: Phone
    address: Option*[Address]
    friends: seq[PersonId]

type
# Struct: LookupPerson
  LookupPerson = object
    id: PersonId

type
# Struct: LookupPersonByName
  LookupPersonByName = object
    name: Name

type
# Struct: InsertPerson
  InsertPerson = object
    person: Person
