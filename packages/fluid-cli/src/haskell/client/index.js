var Lines = require('../../lines.js').Lines;
var httpClient = require('./addon/httpClient.js');

var {
  mkExportTypes,
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
  memberName,
  isFunc,
  genPull,
} = require('../common.js');

const genPragmas = () => {
  return new Lines([
    '-- Pragmas\n',
    '{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n',
    '{-# LANGUAGE DuplicateRecordFields #-}\n',
    '{-# LANGUAGE LambdaCase #-}\n',
    '{-# LANGUAGE OverloadedStrings #-}\n',
    '{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n',
    '{-# LANGUAGE MultiParamTypeClasses #-}\n',
    '{-# LANGUAGE NamedFieldPuns #-}\n',
    '{-# LANGUAGE TupleSections #-}\n',
    '{-# LANGUAGE FlexibleContexts #-}\n',
    '{-# LANGUAGE FlexibleInstances #-}\n',
    '{-# LANGUAGE ScopedTypeVariables #-}\n',
    '{-# LANGUAGE NoImplicitPrelude #-}\n',
  ]);
};

const genModule = (name, lowercaseName, prefix, version, types, values) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '\n',
    '  ( ', lowercaseName, '\'version\n',
    '  , ', lowercaseName, '\'pull\n',
    '  , ', lowercaseName, '\'request\n',
  ]);
  types.forEach(type =>
    lines.add([
      '  , ', type, '(..)\n',
    ])
  );
  values.forEach(value =>
    lines.add([
        '  , ', value, '\n',
    ])
  );
  lines.add('  ) where\n');
  return lines;
};

const genImports = (imports) => {
  var lines = new Lines([
    '\n',
    '-- Imports\n',
    'import qualified Prelude as P\n',
    'import qualified Control.Monad as P\n',
    'import qualified Data.String as P (IsString)\n',
    'import qualified Data.IORef as IO\n',

    'import qualified Fluid.Client as C\n',
    'import qualified Fluid.Client.Expr as C\n',
    'import qualified Fluid.Ast as Ast\n',
    'import qualified Fluid.Imports as R\n'
  ]);
  imports.forEach(x => lines.add(x));
  lines.add('\n');
  return lines;
};

const genRequest = (s) => {
  return new Lines([
    '\n',
    s.lowercaseName, '\'request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => ', s.meta ,' -> C.Expr a -> C.Request ', s.meta,' a\n',
    s.lowercaseName, '\'request _meta _query = C.Request (C.Version ', 0,' ', 0, ') ', s.lowercaseName,'\'version _meta _query\n',
  ]);
};

const genService = (s) => {
  var lines = new Lines();
  s.hollow.filter(isFunc).forEach(call => {
    lines.add([
      '\n',
      s.lowercaseName, '\'', call.name, ' :: C.Expr ', call.output, '\n',
      s.lowercaseName, '\'', call.name, ' = C.unsafeExpr (Ast.Ast\'HollowCall (Ast.HollowCall "', call.label, '"))\n',
    ]);
  });

  s.wrap.filter(isFunc).forEach(call => {
    lines.add([
      '\n',
      s.lowercaseName, '\'', call.name, ' :: C.Expr ', call.name, ' -> C.Expr ', call.output, '\n',
      s.lowercaseName, '\'', call.name, ' = C.unsafeExpr P.. Ast.Ast\'WrapCall P.. Ast.WrapCall "', call.label, '" P.. Ast.toAst\n',
    ]);
  });

  s.struct.filter(isFunc).forEach(call => {
    lines.add([
      '\n',
      s.lowercaseName, '\'', call.name, ' :: C.Expr ', call.name, ' -> C.Expr ', call.output, '\n',
      s.lowercaseName, '\'', call.name, ' = C.unsafeExpr P.. Ast.Ast\'StructCall P.. Ast.StructCall "', call.label, '" P.. Ast.toAst\n',
    ]);
  });

  s.enumeration.filter(isFunc).forEach(call => {
    lines.add([
      '\n',
      s.lowercaseName, '\'', call.name, ' :: C.Expr ', call.name, ' -> C.Expr ', call.output, '\n',
      s.lowercaseName, '\'', call.name, ' = C.unsafeExpr P.. Ast.Ast\'EnumerationCall P.. Ast.EnumerationCall "', call.label, '" P.. Ast.toAst\n',
    ]);
  });

  return lines;
};

const mkExportValues = (s) => {
  var calls =
    [].concat(s.hollow).concat(s.wrap).concat(s.struct).concat(s.enumeration)
      .filter(isFunc)
      .map(x => s.lowercaseName + '\'' + x.name);

  var exprMk =
    [].concat(s.struct).concat(s.wrap)
    .map(x => x.lowercaseName + '\'Mk');
  s.enumeration.forEach(({lowercaseName, enumerals}) =>
    enumerals.forEach(({tag}) =>
      exprMk.push(lowercaseName + '\'' + tag + '\'Mk')
    )
  );

  var expr =
    [].concat(s.struct).concat(s.wrap)
    .map(x => x.lowercaseName + '\'');
  expr = expr.concat(s.enumeration.map(({lowercaseName}) => lowercaseName + '\''));

  var paths = [];
  s.struct.forEach(struct =>
    struct.members.forEach(member =>
      paths.push(struct.lowercaseName + '\'' + member.name)));
  s.enumeration.forEach(({lowercaseName, enumerals}) =>
    enumerals.forEach(({tag, members}) => {
      if (members) {
        members.forEach(member =>
          paths.push(lowercaseName + '\'' + tag + '\'' + member.name)
        );
      }
    }));

  var match = s.enumeration.map(({lowercaseName}) => lowercaseName + '\'Match');

  return calls.concat(exprMk).concat(expr).concat(paths).concat(match);
};

const genWrapExpr = ({name, lowercaseName, type}) => {
  var lines = new Lines();

  lines.add([
    '\n',
    lowercaseName, '\'Mk :: C.Expr (', type,
  ]);
  lines.add([' -> ', name, ')\n']);

  lines.add([
    lowercaseName, '\'Mk = C.unsafeWrapExpr\n',
  ]);

  lines.add([
    '\n',
    lowercaseName, '\' :: ', name,' -> C.Expr ', name, '\n',
    lowercaseName, '\' = C.unsafeExpr P.. Ast.toAst\n',
  ]);

  return lines;
};

const genWrapToAst = ({name}) => {
  var lines = new Lines();
  lines.add([
    '\n',
    'instance Ast.ToAst ', name, ' where\n',
    '  toAst (', name, ' _w) = Ast.toAst _w\n',
  ]);
  return lines;
};

const genStructPath = ({name, lowercaseName, members}) => {
  var lines = new Lines();

  members.forEach(member =>
    lines.add([
      '\n',
      lowercaseName, '\'', member.name, ' :: C.Path (', name, ' -> ', member.type, ')\n',
      lowercaseName, '\'', member.name, ' = C.unsafePath ["', member.label ,'"]\n',
    ])
  );

  return lines;
};

const genStructToAst = ({name, label, members}) => {
  var lines = new Lines();

  lines.add([
    '\n',
    'instance Ast.ToAst ', name, ' where', '\n',
    '  toAst ', name, '\n',
  ]);
  lines.add(['    { ', memberName(name, members[0].name), '\n']);
  members.slice(1).forEach(member =>
    lines.add(['    , ', memberName(name, member.name), '\n'])
  );
  lines.add('    }');
  lines.add([
    ' = Ast.Ast\'Struct P.. Ast.Struct P.$ R.fromList\n',
    '    [ ("', members[0].label, '", Ast.toAst ', memberName(name, members[0].name), ')\n',
  ]);
  members.slice(1).forEach(member =>
    lines.add(['    , ("', member.label, '", Ast.toAst ', memberName(name, member.name), ')\n'])
  );
  lines.add('    ]\n');

  return lines;
};

const genStructExpr = ({name, lowercaseName, members}) => {
  var lines = new Lines();

  lines.add([
    '\n',
    lowercaseName, '\'Mk :: C.Expr (', members[0].type,
  ]);
  members.slice(1).forEach(member =>
    lines.add([' -> ', member.type])
  );
  lines.add([' -> ', name, ')\n']);

  lines.add([
    lowercaseName, '\'Mk = C.unsafeStructExpr ["', members[0].label, '"',
  ]);
  members.slice(1).forEach(member =>
    lines.add([', "', member.label, '"'])
  );
  lines.add(']\n');

  lines.add([
    '\n',
    lowercaseName, '\' :: ', name,' -> C.Expr ', name, '\n',
    lowercaseName, '\' = C.unsafeExpr P.. Ast.toAst\n',
  ]);

  return lines;
};

const genEnumerationPath = ({name, lowercaseName, enumerals}) => {
  var lines = new Lines();
  enumerals.forEach(({tag, members}) => {
    if (members) {
      members.forEach(member =>
        lines.add([
          '\n',
          lowercaseName, '\'', tag, '\'', member.name, ' :: C.Path (', name,'\'', tag, '\'Members -> ', member.type, ')\n',
          lowercaseName, '\'', tag, '\'', member.name, ' = C.unsafePath ["', member.label ,'"]\n',
        ])
      );
    }
  });
  return lines;
};

const genEnumerationMatch = ({name, lowercaseName, enumerals}) => {
  var lines = new Lines();
  lines.add('\n');
  lines.add([
    lowercaseName, '\'Match\n',
    '  :: (C.HasType a, Ast.ToAst a)\n',
    '  => C.Expr ', name, '\n',
  ]);
  enumerals.forEach(({tag, members}) => {
    if (!members) {
      lines.add(['  -> C.Expr a -- ', tag, '\n']);
    } else {
      lines.add(['  -> (C.Symbol, C.Expr ', name, '\'', tag, '\'Members -> C.Expr a) -- | ', tag, '\n']);
    }
  });
  lines.add('  -> C.Expr a\n');
  lines.add([lowercaseName, '\'', 'Match _enumeral']);
  enumerals.forEach(({tag}) => lines.add([' _', tag]));
  lines.add([
    ' = C.unsafeExpr P.$ Ast.Ast\'Match P.$ Ast.Match (Ast.toAst _enumeral)\n',
    '  [ ',
  ]);
  enumerals.forEach(({members, label, tag}, ix) => {
    if (ix !== 0) {
      lines.add('\n  , ');
    }
    if (!members) {
      lines.add(['Ast.MatchCase\'Tag "', label, '" ', '(Ast.toAst _', tag, ')'])
    } else {
      let sym = ['(P.fst _', tag,')'].join('');
      let ref = ['(C.unsafeExpr P.$ Ast.Ast\'Ref P.$ Ast.Ref ', sym,')'].join('');
      lines.add(['Ast.MatchCase\'Members "', label, '" ', sym, ' (Ast.toAst P.$ P.snd _', tag, ' ', ref, ')']);
    }
  });
  lines.add('\n  ]\n');
  return lines;
};

const genEnumerationToAst = ({name, enumerals}) => {
  var lines = new Lines([
    '\n',
    'instance Ast.ToAst ', name, ' where', '\n',
    '  toAst = \\case\n',
  ]);

  enumerals.forEach(enumeral => {
    if (!enumeral.members) {
      lines.add([
        '    ', name, '\'', enumeral.tag, ' -> Ast.Ast\'Enumeral P.$ Ast.Enumeral "', enumeral.label, '" P.Nothing\n',
      ]);
    } else {
      lines.add([
        '    ', name, '\'', enumeral.tag, ' ', name, '\'', enumeral.tag, '\'Members\n',
      ]);
      lines.add([
        '      { ', memberName(name + '\'' + enumeral.tag, enumeral.members[0].name), '\n'
      ]);
      enumeral.members.slice(1).forEach(member =>
        lines.add([
          '      , ', memberName(name + '\'' + enumeral.tag, member.name), '\n'
        ])
      );
      lines.add([
        '      } -> Ast.Ast\'Enumeral P.$ Ast.Enumeral "', enumeral.label, '" P.$ P.Just P.$ R.fromList\n',
      ]);
      lines.add([
        '      [ ("', enumeral.members[0].label, '", Ast.toAst ', memberName(name + '\'' + enumeral.tag, enumeral.members[0].name), ')\n'
      ]);
      enumeral.members.slice(1).forEach(member =>
        lines.add([
          '      , ("', member.label, '", Ast.toAst ', memberName(name + '\'' + enumeral.tag, member.name), ')\n'
        ])
      );
      lines.add('      ]\n');
    }
  });

  return lines;
};

const genEnumeralExpr = ({name, lowercaseName, enumerals}) => {
  var lines = new Lines();

  enumerals.forEach(enumeral => {
    if (!enumeral.members) {
      lines.add([
        '\n',
        lowercaseName, '\'', enumeral.tag,'\'Mk :: C.Expr ', name,
      ]);
      lines.add([
        '\n',
        lowercaseName, '\'', enumeral.tag,'\'Mk = C.unsafeExpr P.. Ast.toAst P.$ ', name, '\'', enumeral.tag,'\n',
      ]);
    } else {
      lines.add([
        '\n',
        lowercaseName, '\'', enumeral.tag,'\'Mk :: C.Expr (', enumeral.members[0].type,
      ]);
      enumeral.members.slice(1).forEach(member =>
        lines.add([' -> ', member.type])
      );
      lines.add([' -> ', name, ')\n']);

      lines.add([
        lowercaseName, '\'', enumeral.tag,'\'Mk = C.unsafeEnumeralExpr "', enumeral.label, '" ["', enumeral.members[0].label, '"',
      ]);
      enumeral.members.slice(1).forEach(member =>
        lines.add([', "', member.label, '"'])
      );
      lines.add(']\n');
    }

  });

  lines.add([
    '\n',
    lowercaseName, '\' :: ', name,' -> C.Expr ', name, '\n',
    lowercaseName, '\' = C.unsafeExpr P.. Ast.toAst\n',
  ]);
  return lines;
};

const genToExpr = ({name}) => {
  return new Lines([
    '\n',
    'instance C.ToExpr ', name, '\n',
  ]);
}


const gen = (specs, addons) => {
  const spec = specs[specs.length - 1];
  const exportTypes = mkExportTypes(spec);
  const exportValues = mkExportValues(spec);

  var addonOptions = {'http-client': httpClient};
  var addonImporting = [];
  var addonExporting = [];
  var addonGen = [];
  addons.forEach(addon => {
    var option = addonOptions[addon];
    if (option) {
      addonImporting = addonImporting.concat(option.importing(spec));
      addonExporting = addonExporting.concat(option.exporting(spec));
      addonGen.push(option.gen(spec));
    }
  });

  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(spec.name, spec.lowercaseName, spec.module, spec.version, exportTypes, exportValues.concat(addonExporting)));
  lines.add(genImports(addonImporting));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Configs\n');
  lines.add('--------------------------------------------------------\n');

  lines.add(genVersion(spec.lowercaseName, spec.version.major, spec.version.minor));
  lines.add(genPull(spec));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Types\n');
  lines.add('--------------------------------------------------------\n');
  spec.wrap.forEach(ty => {
    lines.add(genWrap(ty));
  });
  spec.struct.forEach(ty => {
    lines.add(genStruct(ty));
  });
  spec.enumeration.forEach(ty => {
    lines.add(genEnumeration(ty));
  });

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- API\n');
  lines.add('--------------------------------------------------------\n');

  lines.add(genRequest(spec));
  lines.add(genService(spec));

  spec.wrap.forEach(ty => {
    lines.add(genWrapExpr(ty));
  });
  spec.struct.forEach(ty => {
    lines.add(genStructExpr(ty));
    lines.add(genStructPath(ty));
  });
  spec.enumeration.forEach(ty => {
    lines.add(genEnumeralExpr(ty));
    lines.add(genEnumerationPath(ty));
  });

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Add-ons\n');
  lines.add('--------------------------------------------------------\n');
  addonGen.forEach(gen => lines.add(gen));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Type Instances\n');
  lines.add('--------------------------------------------------------\n');

  spec.wrap.forEach(ty => {
    lines.add(genHasType(ty));
    lines.add(genWrapToVal(ty));
    lines.add(genWrapFromVal(ty));
    lines.add(genToExpr(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
    lines.add(genWrapToAst(ty));
  });
  spec.struct.forEach(ty => {
    lines.add(genHasType(ty));
    lines.add(genStructToVal(ty));
    lines.add(genStructFromVal(ty));
    lines.add(genToExpr(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
    lines.add(genStructToAst(ty));
  });
  spec.enumeration.forEach(ty => {
    lines.add(genHasType(ty));
    ty.enumerals.forEach(enumeral => {
      if (enumeral.members) {
        lines.add(genHasType({name: ty.name + '\'' + enumeral.tag + '\'Members', label: ty.label}))
      }
    });
    lines.add(genEnumerationToVal(ty));
    lines.add(genEnumerationFromVal(ty));
    lines.add(genToExpr(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
    lines.add(genEnumerationToAst(ty));
    lines.add(genEnumerationMatch(ty));
  });
  lines.add('\n');
  return lines.collapse();
};

module.exports = {
  gen
};
