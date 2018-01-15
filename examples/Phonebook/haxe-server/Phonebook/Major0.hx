
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: Address
class Address {
    public var street:Street;
    public var city:City;
    public var zipcode:Zipcode;
    public var state:State;

    public inline function new(street:Street, city:City, zipcode:Zipcode, state:State) {
        this.street = street;
        this.city = city;
        this.zipcode = zipcode;
        this.state = state;
    }
}

// Struct: Person
class Person {
    public var name:Name;
    public var homeNumber:Phone;
    public var cellNumber:Phone;
    public var address:Null<Address>;
    public var friends:Array<PersonId>;

    public inline function new(name:Name, homeNumber:Phone, cellNumber:Phone, address:Null<Address>, friends:Array<PersonId>) {
        this.name = name;
        this.homeNumber = homeNumber;
        this.cellNumber = cellNumber;
        this.address = address;
        this.friends = friends;
    }
}

// Struct: LookupPerson
class LookupPerson {
    public var id:PersonId;

    public inline function new(id:PersonId) {
        this.id = id;
    }
}

// Struct: LookupPersonByName
class LookupPersonByName {
    public var name:Name;

    public inline function new(name:Name) {
        this.name = name;
    }
}

// Struct: InsertPerson
class InsertPerson {
    public var person:Person;

    public inline function new(person:Person) {
        this.person = person;
    }
}
