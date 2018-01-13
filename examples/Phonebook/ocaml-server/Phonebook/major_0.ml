
////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////

// Struct: address
type address = {
  street : street;
  city : city;
  zipcode : zipcode;
  state : state;
}

// Struct: person
type person = {
  name : name;
  home_number : phone;
  cell_number : phone;
  address : address maybe;
  friends : (person_id list);
}

// Struct: lookup_person
type lookup_person = {
  id : person_id;
}

// Struct: lookup_person_by_name
type lookup_person_by_name = {
  name : name;
}

// Struct: insert_person
type insert_person = {
  person : person;
}
