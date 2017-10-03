var Lines = require('../lines').Lines;

const enumeralNameTagMember = (n,e) => n + '\'' + e + '\'Members';

const isFunc = x => x.func && x.output;

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

const genPragmas = () => {
  return new Lines([
    '-- Pragmas\n',
    '{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n'
  ]);
};

const genVersion = (lowercaseName, major, minor) => {
  return new Lines([
    '\n',
    '-- Version\n',
    lowercaseName, '\'Version :: C.Version\n',
    lowercaseName, '\'Version = C.Version ', major, ' ', minor, '\n',
  ]);
};

const genWrap = ({name, label, type, instances}) => {
  var lines = new Lines([
    '\n',
    '-- Wrap: ', name , '\n',
    'newtype ', name , ' = ', name, ' ', type, '\n',
  ]);
  lines.add([
    '  deriving (P.Show, P.Eq, P.Ord, ', instances.text ? 'P.IsString, T.ToText, ' : '', instances.number ? 'P.Num, ' : '', 'A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)', '\n',
  ]);
  lines.add([
    '\n',
    'instance C.HasType ', name,' where\n',
    '  getType _ = "', label,'"\n',
  ]);
  return lines;
};


const genStruct = ({name, label, members}) => {
  var lines = new Lines();

  { // Data type declaration
    lines.add([
      '\n',
      '-- Struct: ', name, '\n',
      'data ', name, ' = ', name, '\n',
    ]);
    lines.add(['  { ', members[0].name, ' :: ', members[0].type, '\n']);
    for (var i = 1; i < members.length; i++) {
      lines.add(['  , ', members[i].name, ' :: ', members[i].type, '\n']);
    }
    lines.add('  } deriving (P.Show, P.Eq, P.Generic)\n');
  }

  { // HasType instance
    lines.add([
      '\n',
      'instance C.HasType ', name,' where\n',
      '  getType _ = "', label,'"\n',
    ]);
  }

  { // ToJSON instance
    lines.add([
      '\n',
      'instance A.ToJSON ', name, '\n',
    ]);
  }

  { // ToVal instance
    lines.add([
      '\n',
      'instance C.ToVal ', name, ' where', '\n',
      '  toVal ', name, '\n',
    ]);
    lines.add(['    { ', members[0].name, '\n']);
    members.slice(1).forEach(member =>
      lines.add(['    , ', member.name, '\n'])
    );
    lines.add('    }');
    lines.add([
      ' = C.Val\'ApiVal P.$ C.ApiVal\'Struct P.$ C.Struct P.$ Map.fromList\n',
      '    [ ("', members[0].label, '", C.toVal ', members[0].name, ')\n',
    ]);
    members.slice(1).forEach(member =>
      lines.add(['    , ("', member.label, '", C.toVal ', member.name, ')\n'])
    );
    lines.add('    ]\n\n');
  }

  { // FromVal instance
    lines.add([
      'instance C.FromVal ', name, ' where\n',
      '  fromVal = \\case\n',
      '    C.Val\'ApiVal (C.ApiVal\'Struct (C.Struct m)) -> ', name, '\n',
      '      P.<$> C.getMember m "', members[0].label , '"\n'
    ]);
    members.slice(1).forEach(member =>
      lines.add([
        '      P.<*> C.getMember m "', member.label, '"\n'
      ])
    );
    lines.add(
      '    _ -> P.Nothing\n'
    );
  }

  return lines;
};


const genEnumeration = ({name, label, enumerals}) => {
  var lines = new Lines();

  function nameTag(tag) {
    return name + '\'' + tag;
  }
  function nameTagMembers(tag) {
    return enumeralNameTagMember(name, tag);
  }

  { // Data type declaration
    lines.add([
      '\n',
      '-- Enumeration: ', name, '\n',
      'data ', name, '\n',
    ]);
    lines.add(['  = ', nameTag(enumerals[0].tag), ' '])
    lines.add(enumerals[0].members ? [nameTagMembers(enumerals[0].tag), '\n'] : ['\n']);
    enumerals.slice(1).forEach(enumeral =>
      lines.add(enumeral.members
        ? ['  | ', nameTag(enumeral.tag), ' ', nameTagMembers(enumeral.tag), '\n']
        : ['  | ', nameTag(enumeral.tag), '\n']
      )
    );
    lines.add('  deriving (P.Show, P.Eq)\n');
  }

  { // HasType instance
    lines.add([
      '\n',
      'instance C.HasType ', name,' where\n',
      '  getType _ = "', label,'"\n',
    ]);
  }

  { // Data type declarations for members
    enumerals.forEach(enumeral => {
      if (enumeral.members) {
        // Data type declarations for member
        lines.add([
          '\n',
          'data ', nameTagMembers(enumeral.tag), ' = ', nameTagMembers(enumeral.tag), '\n'
        ]);
        lines.add(['  { ', enumeral.members[0].name, ' :: ', enumeral.members[0].type, '\n']);
        enumeral.members.slice(1).forEach(member =>
          lines.add(['  , ', member.name, ' :: ', member.type, '\n'])
        );
        lines.add('  } deriving (P.Show, P.Eq, P.Generic)\n');
        // ToJSON instance
        lines.add([
          '\n',
          'instance A.ToJSON ', nameTagMembers(enumeral.tag), '\n'
        ]);
      }
    });
  }

  { // ToJSON instance
    lines.add([
      '\n',
      'instance A.ToJSON ', name, ' where\n',
      '  toJSON = \\case\n',
    ]);
    enumerals.forEach(enumeral => {
      lines.add([
        '    ', nameTag(enumeral.tag), ' ',
      ]);
      if (!enumeral.members) {
        lines.add(['-> A.object [ "tag" A..= ("', enumeral.tag, '" :: T.Text) ]\n']);
      } else {
        lines.add(['m -> C.combineObjects (A.object [ "tag" A..= ("', enumeral.label, '" :: T.Text) ]) (A.toJSON m)\n']);
      }
    });
  }

  function nameTag(tag) {
    return name + '\'' + tag;
  }
  function nameTagMembers(tag) {
    return enumeralNameTagMember(name, tag);
  }

  { // FromVal instance
    lines.add([
      '\n',
      'instance C.FromVal ', name, ' where\n',
      '  fromVal = \\case\n',
      '    C.Val\'ApiVal (C.ApiVal\'Enumeral (C.Enumeral tag m)) -> case (tag,m) of\n',
    ]);
    enumerals.forEach(enumeral => {
      if (!enumeral.members) {
        lines.add([
          '      ("', enumeral.label, '", P.Nothing) -> P.Just ', nameTag(enumeral.tag), '\n',
        ]);
      } else {
        lines.add([
          '      ("', enumeral.label, '", P.Just m\') -> ', nameTag(enumeral.tag), ' P.<$> (', nameTagMembers(enumeral.tag), '\n',
        ]);
        lines.add([
          '          P.<$> C.getMember m\' "', enumeral.members[0].label, '"\n'
        ]);
        enumeral.members.slice(1).forEach(member =>
          lines.add([
            '          P.<*> C.getMember m\' "', member.label, '"\n'
          ])
        );
        lines.add('        )\n');
      }
    });
    lines.add([
      '      _ -> P.Nothing\n',
      '    _ -> P.Nothing\n',
    ]);
  }

  { // ToVal instance
    lines.add([
      '\n',
      'instance C.ToVal ', name, ' where\n',
      '  toVal = \\case\n',
    ]);
    enumerals.forEach(enumeral => {
      if (!enumeral.members) {
        lines.add([
          '    ', nameTag(enumeral.tag), ' -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral "', enumeral.label, '" P.Nothing\n',
        ]);
      } else {
        lines.add([
          '    ', nameTag(enumeral.tag), ' ', nameTagMembers(enumeral.tag), '\n',
        ]);
        lines.add([
          '      { ', enumeral.members[0].name, '\n'
        ]);
        enumeral.members.slice(1).forEach(member =>
          lines.add([
            '      , ', member.name, '\n'
          ])
        );
        lines.add([
          '      } -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral "', enumeral.label, '" P.$ P.Just P.$ Map.fromList\n',
        ]);
        lines.add([
          '      [ ("', enumeral.members[0].label, '", C.toVal ', enumeral.members[0].name, ')\n'
        ]);
        enumeral.members.slice(1).forEach(member =>
          lines.add([
            '      , ("', member.label, '", C.toVal ', member.name, ')\n'
          ])
        );
        lines.add('      ]\n');
      }
    });
  }

  return lines;
};

module.exports = {
  enumeralNameTagMember,
  mkExportTypes,
  mkImportTypes,
  genPragmas,
  genWrap,
  genStruct,
  genEnumeration,
  genVersion,
  isFunc,
};
