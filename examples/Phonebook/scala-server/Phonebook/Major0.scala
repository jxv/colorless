
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////


// Struct: Address
case class Address(street: Street, city: City, zipcode: Zipcode, state: State)

// Struct: Person
case class Person(name: Name, homeNumber: Phone, cellNumber: Phone, address: scala.Option[Address], friends: scala.Array[PersonId])

// Struct: LookupPerson
case class LookupPerson(id: PersonId)

// Struct: LookupPersonByName
case class LookupPersonByName(name: Name)

// Struct: InsertPerson
case class InsertPerson(person: Person)
