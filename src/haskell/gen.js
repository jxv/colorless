const R = require('ramda');
var Lines = require('../lines.js').Lines;

var { enumeralNameTagMember, mkExportTypes, mkImportTypes } = require('./common.js');

const isFunc = x => x.func && x.output;

const genWrap = ({name, type, instances}) => {
  var lines = new Lines([
    '\n',
    '-- Wrap: ', name , '\n',
    'newtype ', name , ' = ', name, ' ', type, '\n',
  ]);
  lines.add([
    '  deriving (P.Show, P.Eq, P.Ord, ', instances.text ? 'P.IsString, T.ToText, ' : '', instances.number ? 'P.Num, ' : '', 'A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)', '\n',
  ]);
  return lines.collapse();
};


const genStruct = ({name, members}) => {
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

  return lines.collapse();
};


const genEnumeration = ({name, enumerals}) => {
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

  return lines.collapse();
};


const genApi = (name, calls) => {
  var lines = new Lines();
  lines.add([
    '\n',
    '-- API: ', name, '\n',
    'data ', name, '\n',
  ]);
  lines.add([
    '  = ', name, '\'', calls[0].name,  calls[0].filled ? (' ' + calls[0].name) : '', '\n',
  ]);
  calls.slice(1).forEach(call =>
    lines.add([
      '  | ', name, '\'', call.name,  call.filled ? (' ' + call.name) : '', '\n'
    ])
  );
  lines.add([
    '  deriving (P.Show, P.Eq)\n',
  ]);
  return lines.collapse();
};


const genServiceThrower = (error) => {
  return new Lines([
    '\n',
    '-- ServiceThrower\n',
    'class P.Monad m => ServiceThrower m where\n',
    '  serviceThrow :: ', error, ' -> m a\n',
  ]).collapse();
};


const genService = (calls) => {
  var lines = new Lines([
    '\n',
    '-- Service\n',
    'class ServiceThrower m => Service meta m where\n'
  ]);
  calls.forEach(call =>
    lines.add([
      '  ', call.func, ' :: meta ->', call.name ? (' ' + call.name + ' ->') : '', ' m ', call.output, '\n',
    ])
  );
  return lines.collapse();
};


const genVersion = (major,minor) => {
  return new Lines([
    '\n',
    '-- Version\n',
    'version :: C.Version\n',
    'version = C.Version ', major, ' ', minor, '\n',
  ]).collapse();
};

const genApiParser = (name, calls) => {
  var lines = new Lines([
    '\n',
    '-- API Parser\n',
    'apiParser :: C.ApiParser ', name, '\n',
    'apiParser = C.ApiParser\n',
  ]);

  // Hollow
  if (calls.hollow.length) {
    lines.add('  { hollow = Map.fromList\n');
    lines.add([
      '     [ ("', calls.hollow[0].label, '", ', name, '\'', calls.hollow[0].name, ')\n',
    ]);
    calls.hollow.slice(1).forEach(hollow =>
      lines.add([
        '     , ("', hollow.label, '", ', name, '\'', hollow.name, ')\n',
      ])
    );
    lines.add('     ]\n');
  } else {
    lines.add('  { hollow = Map.empty\n');
  }

  // Struct
  if (calls.struct.length) {
    lines.add('  , struct = Map.fromList\n');
    lines.add([
      '     [ ("', calls.struct[0].label, '", v ', name, '\'', calls.struct[0].name, ')\n',
    ]);
    calls.struct.slice(1).forEach(struct =>
      lines.add([
        '     , ("', struct.label, '", v ', name, '\'', struct.name, ')\n',
      ])
    );
    lines.add('     ]\n');
  } else {
    lines.add('  , struct = Map.empty\n');
  }

  // Enumeration
  if (calls.enumeration.length) {
    lines.add('  , enumeration = Map.fromList\n');
    lines.add([
      '     [ ("', calls.enumeration[0].label, '", v ', name, '\'', calls.enumeration[0].name, ')\n',
    ]);
    calls.enumeration.slice(1).forEach(enumeration =>
      lines.add([
        '     , ("', enumeration.label, '", v ', name, '\'', enumeration.name, ')\n',
      ])
    );
    lines.add('     ]\n');
  } else {
    lines.add('  , enumeration = Map.empty\n');
  }

  // Wrap
  if (calls.wrap.length) {
    lines.add('  , wrap = Map.fromList\n');
    lines.add([
      '     [ ("', calls.wrap[0].label, '", v ', name, '\'', calls.wrap[0].name, ')\n',
    ]);
    calls.wrap.slice(1).forEach(wrap =>
      lines.add([
        '     , ("', wrap.label, '", v ', name, '\'', wrap.name, ')\n',
      ])
    );
    lines.add('     ]\n');
  } else {
    lines.add('  , wrap = Map.empty\n');
  }

  lines.add([
    '  }\n',
    '  where\n',
    '    v x y = x P.<$> C.fromVal y\n'
  ]);

  return lines.collapse();
};

const genApiLookup = (name, calls) => {
  var lines = [
    '\n',
    '-- API\n',
    'api :: (Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val\n',
    'api meta\' apiCall\' = case C.parseApiCall apiParser apiCall\' of\n',
    '  P.Nothing -> C.runtimeThrow C.RuntimeError\'UnrecognizedCall\n',
    '  P.Just x\' -> case x\' of\n',
  ];
  for (var i = 0; i < calls.hollow.length; i++) {
    lines = lines.concat([
      '    ', name, '\'', calls.hollow[i].name, ' -> C.toVal P.<$> ', calls.hollow[i].func, ' meta\'\n',
    ]);
  }
  for (var i = 0; i < calls.filled.length; i++) {
    lines = lines.concat([
      '    ', name, '\'', calls.filled[i].name, ' a\' -> C.toVal P.<$> ', calls.filled[i].func, ' meta\' a\'\n',
    ]);
  }
  return lines.join('');
};

const genHandleRequest = (meta) => {
  var lines = [
    '\n',
    '-- Handle Request\n',
    'handleRequest :: (Service meta m, C.RuntimeThrower m, IO.MonadIO m) => C.Options -> (', meta, ' -> m meta) -> C.Request -> m C.Response\n',
    'handleRequest options metaMiddleware C.Request{meta,calls} = do\n',
    '  meta\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableMeta) P.return (C.fromValFromJson meta)\n',
    '  xformMeta <- metaMiddleware meta\'\n',
    '  envRef <- IO.liftIO C.emptyEnv\n',
    '  variableBaseCount <- IO.liftIO (Map.size P.<$> IO.readIORef envRef)\n',
    '  let options\' = C.Options\n',
    '        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)\n',
    '        }\n',
    '  let evalConfig = C.EvalConfig\n',
    '        { C.options = options\'\n',
    '        , C.apiCall = api xformMeta\n',
    '        }\n',
    '  calls\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableCalls) P.return (P.mapM C.jsonToExpr calls)\n',
    '  vals <- P.mapM (\\v -> C.runEval (C.forceVal P.=<< C.eval v envRef) evalConfig) calls\'\n',
    '  P.return (C.Response\'Success (A.toJSON vals))\n',
  ];
  return lines.join('');
};

const genImports = (prefix, importTypes) => {
  var lines = [
    '\n',
    '-- Imports\n',
    'import qualified Prelude as P\n',
    'import qualified Data.Map as Map\n',
    'import qualified Control.Monad.IO.Class as IO\n',
    'import qualified Data.Aeson as A\n',
    'import qualified Data.Text as T\n',
    'import qualified Data.Text.Conversions as T\n',
    'import qualified Data.String as P (IsString)\n',
    'import qualified Data.Word as I\n',
    'import qualified Data.Int as I\n',
    'import qualified Data.IORef as IO\n',
    'import qualified GHC.Generics as P (Generic)\n',
    'import qualified Colorless.Types as C\n',
    'import qualified Colorless.Runtime.Expr as C\n',
    'import qualified Colorless.Runtime.Val as C (ToVal(..), FromVal(..), getMember, fromValFromJson, combineObjects)\n',
    '\n',
  ];
  lines = lines.concat(importTypes.map(({ name, major }) =>
    'import ' + prefix + '.V' + major + ' (' + name + '(..))\n'
  ));
  return lines.join('');
};

const genPragmas = () => {
  return [
    '-- Pragmas\n',
    '{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n',
    '{-# LANGUAGE DeriveGeneric #-}\n',
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
  ].join('');
};

const genModule = (prefix, version, types) => {
  var lines = [
    '\n',
    '-- Module\n',
    'module ', prefix, '.V', version.major, '\n',
    '  ( version\n',
    '  , handleRequest\n',
    '  , ServiceThrower(..)\n',
    '  , Service(..)\n',
  ];
  for (var i = 0; i < types.length; i++) {
    lines = lines.concat([
      '  , ', types[i], '(..)\n',
    ]);
  }
  lines = lines.concat([
    '  ) where\n',
  ]);
  return lines.join('');
};

const mkServiceCalls = (s) => {
  return []
    .concat(s.hollow
      .map(x => {
        var copy = Object.assign({}, x);
        delete copy.name;
        return copy;
      }))
    .concat(s.wrap)
    .concat(s.struct)
    .concat(s.enumeration)
    .filter(isFunc);
};

const mkApiLookupPairs = (s) => {
  return {
    hollow: s.hollow.filter(isFunc),
    filled: []
      .concat(s.wrap)
      .concat(s.struct)
      .concat(s.enumeration)
      .filter(isFunc)
  };
};

const mkApiCalls = (s) => {
  const filled = ({name}) => ({name, filled: true});
  const notFilled = ({name}) => ({name, filled: false});
  return []
    .concat(s.hollow.filter(isFunc).map(notFilled))
    .concat(s.struct.filter(isFunc).map(filled))
    .concat(s.enumeration.filter(isFunc).map(filled))
    .concat(s.wrap.filter(isFunc).map(filled));
};

const mkApiParserCalls = (s) => {
  return {
    hollow: s.hollow.filter(isFunc),
    wrap: s.wrap.filter(isFunc),
    struct: s.struct.filter(isFunc),
    enumeration: s.enumeration.filter(isFunc),
  };
};

const currentTypeSource = R.curry((s,ty) => s.typeSource[ty.name] === s.version.major);

const gen = (s) => {
  const exportTypes = mkExportTypes(s);
  const importTypes = mkImportTypes(s);
  const serviceCalls = mkServiceCalls(s);
  const apiLookupPairs = mkApiLookupPairs(s);
  const apiCalls = mkApiCalls(s);
  const apiParserCalls = mkApiParserCalls(s);
  return [
    genPragmas(),
    genModule(s.module, s.version, exportTypes),
    genImports(s.module, importTypes),
    genVersion(s.version.major, s.version.minor),
    genServiceThrower(s.error),
    genService(serviceCalls),
    genHandleRequest(s.meta),
    genApiLookup(s.name, apiLookupPairs),
    genApiParser(s.name, apiParserCalls),
    genApi(s.name, apiCalls),
  ]
    .concat(s.wrap.filter(currentTypeSource(s)).map(genWrap))
    .concat(s.struct.filter(currentTypeSource(s)).map(genStruct))
    .concat(s.enumeration.filter(currentTypeSource(s)).map(genEnumeration))
    .concat(['\n'])
    .join('');
};

module.exports.gen = gen;
