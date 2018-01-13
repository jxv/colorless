
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
interface Address {
  street: Street;
  city: City;
  zipcode: Zipcode;
  state: State;
}

// Struct: Person
interface Person {
  name: Name;
  homeNumber: Phone;
  cellNumber: Phone;
  address: (Address | null);
  friends: Array<PersonId>;
}

// Struct: LookupPerson
interface LookupPerson {
  id: PersonId;
}

// Struct: LookupPersonByName
interface LookupPersonByName {
  name: Name;
}

// Struct: InsertPerson
interface InsertPerson {
  person: Person;
}