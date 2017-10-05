import { assert } from '../../colorless';

export const version = {
  major: 2,
  minor: 0,
};

export const Hello = (m) => {
  assert(m !== undefined, "`Hello` missing members");
  assert(m.who !== undefined, "`Hello` missing member `who`");
  return {
    n: 'Hello',
    m: {
      'who': m.who,
    },
  };
};

export const Goodbye = (m) => {
  assert(m !== undefined, "`Goodbye` missing members");
  assert(m.target !== undefined, "`Goodbye` missing member `target`");
  return {
    n: 'Goodbye',
    m: {
      'target': m.target,
    },
  };
};
