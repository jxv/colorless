
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////


// Struct: Address
type Address struct {
    street Street
    city City
    zipcode Zipcode
    state State
}

// Struct: Person
type Person struct {
    name Name
    home_number Phone
    cell_number Phone
    address Address*
    friends List
}

// Struct: LookupPerson
type LookupPerson struct {
    id PersonId
}

// Struct: LookupPersonByName
type LookupPersonByName struct {
    name Name
}

// Struct: InsertPerson
type InsertPerson struct {
    person Person
}
