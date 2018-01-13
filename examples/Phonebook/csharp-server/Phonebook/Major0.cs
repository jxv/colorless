
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
public struct Address
{
    public Street street;
    public City city;
    public Zipcode zipcode;
    public State state;

    public Address(Street _street, City _city, Zipcode _zipcode, State _state)
    {
        street = _street;
        city = _city;
        zipcode = _zipcode;
        state = _state;
    }
}

// Struct: Person
public struct Person
{
    public Name name;
    public Phone homeNumber;
    public Phone cellNumber;
    public Option<Address> address;
    public ArrayList<PersonId> friends;

    public Person(Name _name, Phone _homeNumber, Phone _cellNumber, Option<Address> _address, ArrayList<PersonId> _friends)
    {
        name = _name;
        homeNumber = _homeNumber;
        cellNumber = _cellNumber;
        address = _address;
        friends = _friends;
    }
}

// Struct: LookupPerson
public struct LookupPerson
{
    public PersonId id;

    public LookupPerson(PersonId _id)
    {
        id = _id;
    }
}

// Struct: LookupPersonByName
public struct LookupPersonByName
{
    public Name name;

    public LookupPersonByName(Name _name)
    {
        name = _name;
    }
}

// Struct: InsertPerson
public struct InsertPerson
{
    public Person person;

    public InsertPerson(Person _person)
    {
        person = _person;
    }
}
