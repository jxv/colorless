
--------------------------------------------------------
-- Types
--------------------------------------------------------


// Struct: Address
public class Address {
    public Street street;
    public City city;
    public Zipcode zipcode;
    public State state;
}

// Struct: Person
public class Person {
    public Name name;
    public Phone phone;
    public (Maybe Address) address;
    public (List PersonId) friends;
}

// Struct: LookupPerson
public class LookupPerson {
    public PersonId id;
}

// Struct: LookupPersonByName
public class LookupPersonByName {
    public Name name;
}

// Struct: InsertPerson
public class InsertPerson {
    public Person person;
}
