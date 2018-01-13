
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
data class Address(val street: Street, val city: City, val zipcode: Zipcode, val state: State)

// Struct: Person
data class Person(val name: Name, val homeNumber: Phone, val cellNumber: Phone, val address: Address?, val friends: List<PersonId>)

// Struct: LookupPerson
data class LookupPerson(val id: PersonId)

// Struct: LookupPersonByName
data class LookupPersonByName(val name: Name)

// Struct: InsertPerson
data class InsertPerson(val person: Person)
