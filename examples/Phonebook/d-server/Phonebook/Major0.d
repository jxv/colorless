
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
struct Address
{
    Street street;
    City city;
    Zipcode zipcode;
    State state;

    this(Street _street, City _city, Zipcode _zipcode, State _state)
    {
        street = _street;
        city = _city;
        zipcode = _zipcode;
        state = _state;
    }
}

// Struct: Person
struct Person
{
    Name name;
    Phone phone;
    std.typecons.Nullable!(Address) address;
    std.container.dlist.DList!(PersonId) friends;

    this(Name _name, Phone _phone, std.typecons.Nullable!(Address) _address, std.container.dlist.DList!(PersonId) _friends)
    {
        name = _name;
        phone = _phone;
        address = _address;
        friends = _friends;
    }
}

// Struct: LookupPerson
struct LookupPerson
{
    PersonId id;

    this(PersonId _id)
    {
        id = _id;
    }
}

// Struct: LookupPersonByName
struct LookupPersonByName
{
    Name name;

    this(Name _name)
    {
        name = _name;
    }
}

// Struct: InsertPerson
struct InsertPerson
{
    Person person;

    this(Person _person)
    {
        person = _person;
    }
}
