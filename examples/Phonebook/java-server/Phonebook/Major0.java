
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
public class Address {
    public Street street;
    public City city;
    public Zipcode zipcode;
    public State state;

    public Address(Street _street, City _city, Zipcode _zipcode, State _state) {
        street = _street;
        city = _city;
        zipcode = _zipcode;
        state = _state;
    }
}

// Struct: Person
public class Person {
    public Name name;
    public Phone phone;
    public Address? address;
    public List<PersonId> friends;

    public Person(Name _name, Phone _phone, Address? _address, List<PersonId> _friends) {
        name = _name;
        phone = _phone;
        address = _address;
        friends = _friends;
    }
}

// Struct: LookupPerson
public class LookupPerson {
    public PersonId id;

    public LookupPerson(PersonId _id) {
        id = _id;
    }
}

// Struct: LookupPersonByName
public class LookupPersonByName {
    public Name name;

    public LookupPersonByName(Name _name) {
        name = _name;
    }
}

// Struct: InsertPerson
public class InsertPerson {
    public Person person;

    public InsertPerson(Person _person) {
        person = _person;
    }
}
