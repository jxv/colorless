
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

/* Struct: Address */
type Address = {
  street: Street,
  city: City,
  zipcode: Zipcode,
  state: State
}

/* Struct: Person */
type Person = {
  name: Name,
  homeNumber: Phone,
  cellNumber: Phone,
  address: option(Address),
  friends: list(PersonId)
}

/* Struct: LookupPerson */
type LookupPerson = {
  id: PersonId
}

/* Struct: LookupPersonByName */
type LookupPersonByName = {
  name: Name
}

/* Struct: InsertPerson */
type InsertPerson = {
  person: Person
}
