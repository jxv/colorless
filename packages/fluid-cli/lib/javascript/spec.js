var R = require('ramda');

const lowercaseFirstLetter = s => s.charAt(0).toLowerCase() + s.slice(1);

const langTypeName = n => n; // convert types to javascript acceptable names.

const langTypeLabel = n => n; // convert types to javascript acceptable names. Nearly always identity.

const langType = ty => ty;

const hollow = types => types.filter(type => type.n && !type.m && !type.e && !type.w).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o)
}));

const struct = types => types.filter(type => type.n && type.m).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  members: type.m.map(member => {
    const key = Object.keys(member);
    return {
      name: langTypeName(key),
      label: langTypeLabel(key),
      type: langType(member[key])
    };
  }),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o)
}));

const enumeration = types => types.filter(type => type.n && type.e).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  enumerals: type.e.map(enumeral => {
    if (typeof enumeral === 'string') {
      return {
        tag: langTypeName(enumeral),
        label: langTypeLabel(enumeral)
      };
    }
    if (typeof enumeral === 'object') {
      return {
        tag: langTypeName(enumeral.tag),
        label: langTypeLabel(enumeral.tag),
        members: enumeral.m && enumeral.m.map(member => {
          const key = Object.keys(member);
          return {
            name: langTypeName(key),
            label: langTypeLabel(key),
            type: langType(member[key])
          };
        })
      };
    }
  }),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o)
}));

const wrap = types => types.filter(type => type.n && type.w).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  type: langType(type.w),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o)
}));

const spec = (prefix, version, s) => ({
  version: version,
  error: s.pull.error,
  meta: s.pull.meta,
  name: s.pull.name,
  hollow: hollow(s.types),
  struct: struct(s.types),
  enumeration: enumeration(s.types),
  wrap: wrap(s.types)
});

module.exports = {
  spec
};