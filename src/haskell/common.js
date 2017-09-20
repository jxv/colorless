
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

module.exports = {
  enumeralNameTagMember: enumeralNameTagMember,
  mkExportTypes: mkExportTypes,
};
