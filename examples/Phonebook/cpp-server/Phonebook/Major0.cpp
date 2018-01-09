
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
struct Address;

// Struct: Person
struct Person;

// Struct: LookupPerson
struct LookupPerson;

// Struct: LookupPersonByName
struct LookupPersonByName;

// Struct: InsertPerson
struct InsertPerson;

// Struct: Address
struct Address {
   Street street;
   City city;
   Zipcode zipcode;
   State state;
};

// Struct: Person
struct Person {
   Name name;
   Phone phone;
   (Address | Nil) address;
   Array(PersonId) friends;
};

// Struct: LookupPerson
struct LookupPerson {
   PersonId id;
};

// Struct: LookupPersonByName
struct LookupPersonByName {
   Name name;
};

// Struct: InsertPerson
struct InsertPerson {
   Person person;
};
