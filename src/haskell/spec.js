var R = require('ramda');

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
  'String': 'R.Text',
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

const langTypeNameVersion = R.curry((major, n) => primMap[n] || ('V' + major + '.' + n)); // convert types to haskell acceptable names and qualified by version.

const langTypeLabel = n => n; // convert types to haskell acceptable names. Nearly always identity.

const langTypeGeneric = R.curry((tyName, ty) => {
  if (typeof ty === 'string') {
    return tyName(ty);
  }
  if (typeof ty === 'object') {
    if (ty.n === 'List') {
      return '[' + langTypeGeneric(tyName, ty.p) + ']'
    }
    if (ty.n === 'Option') {
      return '(P.Maybe ' + langTypeGeneric(tyName, ty.p) + ')'
    }
    if (ty.n === 'Either') {
      return '(P.Either (' + langTypeGeneric(tyName, ty.p[0]) + ') (' + langTypeGeneric(tyName, ty.p[1]) + '))'
    }
  }
});

const langType = langTypeGeneric(langTypeName);

const langTypeVersion = (major, n) => langTypeGeneric(langTypeNameVersion(major))(n);

const hollow = (types) => types.filter(type => type.n && !type.m && !type.e && !type.w).map(type => ({
  name: langTypeName(type.n),
  label: langTypeLabel(type.n),
  lowercaseName: lowercaseFirstLetter(langTypeName(type.n)),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o),
}));

const struct = (types) => types.filter(type => type.n && type.m).map(type => ({
  name: langTypeName(type.n),
  lowercaseName: lowercaseFirstLetter(langTypeName(type.n)),
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
  lowercaseName: lowercaseFirstLetter(langTypeName(type.n)),
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
        members: enumeral.m && enumeral.m.map(member => {
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
  lowercaseName: lowercaseFirstLetter(langTypeName(type.n)),
  type: langType(type.w),
  func: type.o && lowercaseFirstLetter(langTypeName(type.n)),
  output: langType(type.o),
  instances: {
    text: isString(type.w),
    number: isNumber(type.w),
  },
}));

const spec = (prefix, version, s) => {
  return ({
    module: prefix,
    version: version,
    error: langType(s.pull.error),
    meta: langType(s.pull.meta),
    metaVersion: langTypeVersion(version.major, s.pull.meta),
    name: s.pull.name,
    lowercaseName: lowercaseFirstLetter(s.pull.name),
    hollow: hollow(s.types),
    struct: struct(s.types),
    enumeration: enumeration(s.types),
    wrap: wrap(s.types),
    pull: {
      protocol: s.pull.protocol,
      address: s.pull.address,
      port: s.pull.port,
      path: s.pull.path,
    },
  });
}

module.exports = {
  spec: spec,
};
