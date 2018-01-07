
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////


// Wrap: PersonId
typealias PersonId = String

// Wrap: Name
typealias Name = String

// Wrap: Phone
typealias Phone = String

// Wrap: Street
typealias Street = String

// Wrap: City
typealias City = String

// Wrap: Zipcode
typealias Zipcode = String

// Struct: Address
struct Address {
    let street: Street
    let city: City
    let zipcode: Zipcode
    let state: State
}

// Struct: Person
struct Person {
    let name: Name
    let phone: Phone
    let address: Option<Address>
    let friends: Array<PersonId>
}

// Struct: LookupPerson
struct LookupPerson {
    let id: PersonId
}

// Struct: LookupPersonByName
struct LookupPersonByName {
    let name: Name
}

// Struct: InsertPerson
struct InsertPerson {
    let person: Person
}

// Enumeration: State
enum State {
    CA,
    NY,
    TX,
    Other(State_Other),
}
