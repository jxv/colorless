
########################################################
## Types
########################################################

# Struct: Address
struct Address
  property street, city, zipcode, state
  def initialize(@street : Street, @city : City, @zipcode : Zipcode, @state : State)
  end
end

# Struct: Person
struct Person
  property name, home_number, cell_number, address, friends
  def initialize(@name : Name, @home_number : Phone, @cell_number : Phone, @address : (Address | Nil), @friends : Array(PersonId))
  end
end

# Struct: LookupPerson
struct LookupPerson
  property id
  def initialize(@id : PersonId)
  end
end

# Struct: LookupPersonByName
struct LookupPersonByName
  property name
  def initialize(@name : Name)
  end
end

# Struct: InsertPerson
struct InsertPerson
  property person
  def initialize(@person : Person)
  end
end
