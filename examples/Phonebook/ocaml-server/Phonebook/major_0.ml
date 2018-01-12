
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
type Address = {
  street : Street;
  city : City;
  zipcode : Zipcode;
  state : State;
}

// Struct: Person
type Person = {
  name : Name;
  phone : Phone;
  address : Address maybe;
  friends : (PersonId list);
}

// Struct: LookupPerson
type LookupPerson = {
  id : PersonId;
}

// Struct: LookupPersonByName
type LookupPersonByName = {
  name : Name;
}

// Struct: InsertPerson
type InsertPerson = {
  person : Person;
}
