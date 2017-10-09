import { assert } from '../../colorless';

export const version = {
  major: 0,
  minor: 0,
};

export const LookupPerson = (m) => {
  assert(m !== undefined, "`LookupPerson` missing members");
  assert(m.id !== undefined, "`LookupPerson` missing member `id`");
  return {
    n: 'LookupPerson',
    m: {
      'id': m.id,
    },
  };
};

export const LookupPersonByName = (m) => {
  assert(m !== undefined, "`LookupPersonByName` missing members");
  assert(m.name !== undefined, "`LookupPersonByName` missing member `name`");
  return {
    n: 'LookupPersonByName',
    m: {
      'name': m.name,
    },
  };
};

export const InsertPerson = (m) => {
  assert(m !== undefined, "`InsertPerson` missing members");
  assert(m.person !== undefined, "`InsertPerson` missing member `person`");
  return {
    n: 'InsertPerson',
    m: {
      'person': m.person,
    },
  };
};
