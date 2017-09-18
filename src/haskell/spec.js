const primMap = {
  'Unit': '()',
  'Bool': 'P.Bool',
  'U8': 'I.Word8',
  'U16': 'I.Word16',
  'U32': 'I.Word32',
  'U64': 'I.Word64',
  'I8': 'I.Int8',
  'I16': 'I.Int16',
  'I32': 'I.Int32',
  'I64': 'I.Int64',
  'F32': 'P.Float',
  'F64': 'P.Float',
  'Char': 'P.Char',
  'String': 'T.Text',
};

const lowercaseFirstLetter = s => s.charAt(0).toLowerCase() + s.slice(1);

const isString = n => n === 'String';

const isNumber = n => (
  n === 'I8' ||
  n === 'I16' ||
  n === 'I32' ||
  n === 'I64' ||

  n === 'U8' ||
  n === 'U16' ||
  n === 'U32' ||
  n === 'U64' ||

  n === 'F32' ||
  n === 'F64'
);

const langTypeName = n => primMap[n] || n; // convert types to haskell acceptable names.

const langTypeLabel = n => n; // convert types to haskell acceptable names. Nearly always identity.

const langType = ty => {
  if (typeof ty === 'string') {
    return langTypeName(ty);
  }
  if (typeof ty === 'object') {
    if (ty.n === 'List') {
      return '[' + langType(ty.p) + ']'
    }
    if (ty.n === 'Option') {
      return '(P.Maybe ' + langType(ty.p) + ')'
    }
    if (ty.n === 'Either') {
      return '(P.Either (' + langType(ty.p[0]) + ') (' + langType(ty.p[1]) + '))'
    }
  }
};

const hollow = (types) => types.filter(type => type.n && !type.m && !type.e && !type.w).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o),
}));

const struct = (types) => types.filter(type => type.n && type.m).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  members: type.m.map(member => {
    const key = Object.keys(member);
    return {
      name: langTypeName(key),
      label: langTypeLabel(key),
      type: langType(member[key]),
    }
  }),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o),
}));

const enumeration = (types) => types.filter(type => type.n && type.e).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  enumerals: type.e.map(enumeral => {
    if (typeof enumeral === 'string') {
      return {
        tag: langTypeName(enumeral),
        label: langTypeLabel(enumeral),
      }
    }
    if (typeof enumeral === 'object') {
      return {
        tag: langTypeName(enumeral.tag),
        label: langTypeLabel(enumeral.tag),
        members: enumeral.m.map(member => {
          const key = Object.keys(member);
          return {
            name: langTypeName(key),
            label: langTypeLabel(key),
            type: langType(member[key]),
          }
        }),
      }
    }
  }),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o),
}));

const wrap = (types) => types.filter(type => type.n && type.w).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  type: langType(type.w),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o),
  instances: {
    text: isString(type.w),
    number: isNumber(type.w),
  },
}));

const spec = (prefix, s) => ({
  module: prefix,
  version: { major: 0, minor: 0 },
  error: langType(s.pull.error),
  meta: langType(s.pull.meta),
  name: 'Api', // s.pull.name, // FIXME
  hollow: hollow(s.types),
  struct: struct(s.types),
  enumeration: enumeration(s.types),
  wrap: wrap(s.types),
});

module.exports = {
  spec: spec,
};
