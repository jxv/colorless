import { assert } from '../../fluid';

export const version = {
  major: 0,
  minor: 0,
};

export const Hello = (m) => {
  assert(m !== undefined, "`Hello` missing members");
  assert(m.target !== undefined, "`Hello` missing member `target`");
  return {
    n: 'Hello',
    m: {
      'target': m.target,
    },
  };
};

export default {
  Hello,
};
