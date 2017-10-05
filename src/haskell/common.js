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

const genHasType = ({name, label}) => {
  return new Lines([
    '\n',
    'instance C.HasType ', name,' where\n',
    '  getType _ = "', label,'"\n',
  ]);
};

const genWrap = ({name, type, label, instances}) => {
  var lines = new Lines([
    '\n',
    '-- Wrap: ', name , '\n',
    'newtype ', name , ' = ', name, ' ', type, '\n',
  ]);
  lines.add([
    '  deriving (P.Eq, P.Ord, ', instances.text ? 'P.IsString, T.ToText, ' : '', instances.number ? 'P.Num, ' : '', ' P.Show)\n',
  ]);
  return lines;
};

const genToJson = ({name}) => {
 return new Lines([
    '\n',
    'instance A.ToJSON ', name,' where\n',
    '  toJSON = A.toJSON P.. C.toVal\n',
  ]);
};

const genFromJson = ({name}) => {
 return new Lines([
    '\n',
    'instance A.FromJSON ', name,' where\n',
    '  parseJSON _v = do\n',
    '    _x <- A.parseJSON _v\n',
    '    case C.fromVal _x of\n',
    '      P.Nothing -> P.mzero\n',
    '      P.Just _y -> P.return _y\n',
  ]);
};

const genWrapFromVal = ({name}) => {
  return new Lines([
    '\n',
    'instance C.FromVal ', name,' where\n',
    '  fromVal _v = ', name, ' P.<$> C.fromVal _v\n',
  ]);
};

const genWrapToVal = ({name}) => {
 return new Lines([
    '\n',
    'instance C.ToVal ', name,' where\n',
    '  toVal (', name, ' _w) = C.toVal _w\n',
  ]);
};

const genStruct = ({name, label, members}) => {
  var lines = new Lines();
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
  return lines;
};

const genStructToJson = ({name}) => {
  return new Lines([
      '\n',
      'instance A.ToJSON ', name, '\n',
  ]);
};

const genStructToVal = ({name, label, members}) => {
  var lines = new Lines([
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
  return lines;
};

const genStructFromVal = ({name, label, members}) => {
  var lines = new Lines([
    'instance C.FromVal ', name, ' where\n',
    '  fromVal = \\case\n',
    '    C.Val\'ApiVal (C.ApiVal\'Struct (C.Struct _m)) -> ', name, '\n',
    '      P.<$> C.getMember _m "', members[0].label , '"\n'
  ]);
  members.slice(1).forEach(member =>
    lines.add([
      '      P.<*> C.getMember _m "', member.label, '"\n'
    ])
  );
  lines.add(
    '    _ -> P.Nothing\n'
  );
  return lines;
};

const genEnumeration = ({name, label, enumerals}) => {
  var lines = new Lines();
  lines.add([
    '\n',
    '-- Enumeration: ', name, '\n',
    'data ', name, '\n',
  ]);
  lines.add(['  = ', name, '\'', enumerals[0].tag, ' '])
  lines.add(enumerals[0].members ? [name, '\'', enumerals[0].tag, '\'Members\n'] : ['\n']);
  enumerals.slice(1).forEach(enumeral =>
    lines.add(enumeral.members
      ? ['  | ', name, '\'', enumeral.tag, ' ', name, '\'', enumeral.tag, '\'Members\n']
      : ['  | ', name, '\'', enumeral.tag, '\n']
    )
  );
  lines.add('  deriving (P.Show, P.Eq)\n');
  enumerals.forEach(enumeral => {
    if (enumeral.members) {
      // Data type declarations for member
      lines.add([
        '\n',
        'data ', name, '\'', enumeral.tag, '\'Members = ', name, '\'', enumeral.tag, '\'Members\n'
      ]);
      lines.add(['  { ', enumeral.members[0].name, ' :: ', enumeral.members[0].type, '\n']);
      enumeral.members.slice(1).forEach(member =>
        lines.add(['  , ', member.name, ' :: ', member.type, '\n'])
      );
      lines.add('  } deriving (P.Show, P.Eq, P.Generic)\n');
    }
  });
  return lines;
}

const genEnumerationToJson = ({name, label, enumerals}) => {
  var lines = new Lines([
    '\n',
    'instance A.ToJSON ', name, ' where\n',
    '  toJSON = \\case\n',
  ]);
  enumerals.forEach(enumeral => {
    lines.add([
      '    ', name, '\'', enumeral.tag, ' ',
    ]);
    if (!enumeral.members) {
      lines.add(['-> A.object [ "tag" A..= ("', enumeral.tag, '" :: T.Text) ]\n']);
    } else {
      lines.add(['m -> C.combineObjects (A.object [ "tag" A..= ("', enumeral.label, '" :: T.Text) ]) (A.toJSON m)\n']);
    }
  });
  enumerals.forEach(enumeral => {
    if (enumeral.members) {
      lines.add([
        '\n',
        'instance A.ToJSON ', name, '\'', enumeral.tag, '\'Members\n'
      ]);
    }
  });
  return lines;
};

const genEnumerationFromVal = ({name, enumerals}) => {
  var lines = new Lines([
    '\n',
    'instance C.FromVal ', name, ' where\n',
    '  fromVal = \\case\n',
    '    C.Val\'ApiVal (C.ApiVal\'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of\n',
  ]);
  enumerals.forEach(enumeral => {
    if (!enumeral.members) {
      lines.add([
        '      ("', enumeral.label, '", P.Nothing) -> P.Just ', name, '\'', enumeral.tag, '\n',
      ]);
    } else {
      lines.add([
        '      ("', enumeral.label, '", P.Just _m\') -> ', name, '\'', enumeral.tag, ' P.<$> (', name, '\'', enumeral.tag, '\'Members\n',
      ]);
      lines.add([
        '          P.<$> C.getMember _m\' "', enumeral.members[0].label, '"\n'
      ]);
      enumeral.members.slice(1).forEach(member =>
        lines.add([
          '          P.<*> C.getMember _m\' "', member.label, '"\n'
        ])
      );
      lines.add('        )\n');
    }
  });
  lines.add([
    '      _ -> P.Nothing\n',
    '    _ -> P.Nothing\n',
  ]);
  return lines;
};

const genEnumerationToVal = ({name, enumerals}) => {
  var lines = new Lines([
    '\n',
    'instance C.ToVal ', name, ' where\n',
    '  toVal = \\case\n',
  ]);
  enumerals.forEach(enumeral => {
    if (!enumeral.members) {
      lines.add([
        '    ', name, '\'', enumeral.tag, ' -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral "', enumeral.label, '" P.Nothing\n',
      ]);
    } else {
      lines.add([
        '    ', name, '\'', enumeral.tag, ' ', name, '\'', enumeral.tag, '\'Members', '\n',
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
  return lines;
};

const genPull = ({lowercaseName, pull}) => {
  var lines = new Lines();
  lines.add([
    '\n',
    lowercaseName, '\'Pull :: C.Pull\n',
    lowercaseName, '\'Pull = C.Pull "', pull.protocol, '" "', pull.address, '" "', pull.path, '" ', pull.port, '\n',
  ]);
  return lines;
};

module.exports = {
  enumeralNameTagMember,
  mkExportTypes,
  mkImportTypes,
  genPragmas,
  genHasType,
  genToJson,
  genFromJson,
  genWrap,
  genWrapToVal,
  genWrapFromVal,
  genStruct,
  genStructToVal,
  genStructFromVal,
  genEnumeration,
  genEnumerationToVal,
  genEnumerationFromVal,
  genVersion,
  isFunc,
  genPull,
};
