mod hello_world::major_0 {

////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////


// Wrap: PersonId
#[derive(Debug)]
struct PersonId(str);

// Wrap: Name
#[derive(Debug)]
struct Name(str);

// Wrap: Phone
#[derive(Debug)]
struct Phone(str);

// Wrap: Street
#[derive(Debug)]
struct Street(str);

// Wrap: City
#[derive(Debug)]
struct City(str);

// Wrap: Zipcode
#[derive(Debug)]
struct Zipcode(str);

// Struct: Address
#[derive(Debug)]
struct Address {
    street: Street,
    city: City,
    zipcode: Zipcode,
    state: State,
}

// Struct: Person
#[derive(Debug)]
struct Person {
    name: Name,
    phone: Phone,
    address: std::Option<Address>,
    friends: std::collections::LinkedList<PersonId>,
}

// Struct: LookupPerson
#[derive(Debug)]
struct LookupPerson {
    id: PersonId,
}

// Struct: LookupPersonByName
#[derive(Debug)]
struct LookupPersonByName {
    name: Name,
}

// Struct: InsertPerson
#[derive(Debug)]
struct InsertPerson {
    person: Person,
}

// Enumeration: State
#[derive(Debug)]
enum State {
    CA,
    NY,
    TX,
    Other { name: str, },
}
}