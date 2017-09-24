
const enumeralNameTagMember = (n,e) => n + '\'' + e + '\'Members';

const mkExportTypes = (s) => {
  return []
    .concat(s.wrap.map(x => x.name))
    .concat(s.struct.map(x => x.name))
    .concat([].concat.apply([], s.enumeration.map(e =>
      [e.name]
        .concat(
          e.enumerals
            .filter(x => x.members)
            .map(x => enumeralNameTagMember(e.name, x.tag)))
    )))
};

const mkImportTypes = (s) => {
  const differentMajorVersion = ty => s.typeSource[ty.name] !== s.version.major;
  return []
    .concat(s.wrap
      .filter(differentMajorVersion)
      .map(ty => ({ name: ty.name, major: s.typeSource[ty.name] })))
    .concat(s.struct
      .filter(differentMajorVersion)
      .map(ty => ({ name: ty.name, major: s.typeSource[ty.name] })))
    .concat([].concat.apply([], s.enumeration
      .filter(differentMajorVersion)
      .map(e =>
        [{ name: e.name, major: s.typeSource[e.name] }]
          .concat(
            e.enumerals
              .filter(x => x.members)
              .map(x => ({ name: enumeralNameTagMember(e.name, x.tag), major: s.typeSource[e.name] }))))))
};

module.exports = {
  enumeralNameTagMember,
  mkExportTypes,
  mkImportTypes,
};
