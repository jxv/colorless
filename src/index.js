var fs = require('fs');
var program = require('commander');

program
  .version('0.0.0')
  .option('-s --src [type]', 'Directory of colorless specs')
  .option('-d --dest [type]', 'Directory to generate code')
  .option('-l --lang [type]', 'Language of code')
  .option('-m --prefix [type]', 'Module name/prefix')
  .option('-e --side [type]', 'Client or server side code', 'client')
  .parse(process.argv);

const src = program.src;
const dest = program.dest;
const lang = program.lang;
const side = program.side;
const prefix = program.prefix;

const haskell = {
  primMap: {
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
  },


  wrap(name, type) {
    var lines = [
      '\n',
      '-- Wrap: ', name , '\n',
      'newtype ', name , ' = ', name, ' ', type, '\n',
    ];
    lines = lines.concat([
      '  deriving (P.Show, P.Eq, A.FromJSON, A.ToJSON, C.ToVal, C.FromVal)', '\n',
    ]);
    return lines.join('');
  },


  struct(name, members) {
    // Data type declaration
    const declName = [
      '\n',
      '-- Struct: ', name, '\n',
      'data ', name, ' = ', name, '\n',
    ];
    var declMembers = ['  { ', members[0].name, ' :: ', members[0].type, '\n'];
    for (var i = 1; i < members.length; i++) {
      declMembers = declMembers.concat(['  , ', members[i].name, ' :: ', members[i].type, '\n']);
    }
    var declDeriving = '  } deriving (P.Show, P.Eq, P.Generic)\n\n';
    const decl = declName.concat(declMembers).concat([declDeriving]);

    // ToJSON instance
    const toJSON  = [
      '\n',
      'instance A.ToJSON ', name, '\n',
    ];

    // ToVal instance
    const toValDecl = [
      '\n',
      'instance C.ToVal ', name, ' where', '\n',
      '  toVal ', name, '\n',
    ];
    var toValDecons = ['    { ', members[0].name, '\n'];
    for (var i = 1; i < members.length; i++) {
      toValDecons = toValDecons.concat(['    , ', members[i].name, '\n']);
    }
    toValDecons.push('    }');
    var toValDef = [
      ' = C.Val\'ApiVal P.$ C.ApiVal\'Struct P.$ C.Struct P.$ Map.fromList\n',
      '    [ ("', members[0].label, '", C.toVal ', members[0].name, ')\n',
    ];
    for (var i = 1; i < members.length; i++) {
      toValDef = toValDef.concat(['    , ("', members[i].label, '", C.toVal ', members[i].name, ')\n']);
    }
    toValDef.push('    ]\n\n');
    const toVal = toValDecl.concat(toValDecons).concat(toValDef);

    // FromVal instance
    var fromVal = [
      'instance C.FromVal ', name, ' where\n',
      '  fromVal = \\case\n',
      '    C.Val\'ApiVal (C.ApiVal\'Struct (C.Struct m)) -> ', name, '\n',
      '      P.<$> C.getMember m "', members[0].label , '"\n'
    ];
    for (var i = 1; i < members.length; i++) {
      fromVal = fromVal.concat([
        '      P.<*> C.getMember m "', members[i].label, '"\n'
      ]);
    }
    fromVal = fromVal.concat([
      '    _ -> Nothing\n',
    ]);

    return decl.concat(toJSON).concat(toVal).concat(fromVal).join('');
  },


  enumeration(name, enumerals) {
    // Data type declaration
    const declName = [
      '\n',
      '-- Enumeration: ', name, '\n',
      'data ', name, '\n',
    ];
    function nameTag(i) {
      return name + '\'' + enumerals[i][0];
    }
    function nameTagMembers(i) {
      return name + '\'' + enumerals[i][0] + '\'Members';
    }
    var declEnumerals = ['  = ', nameTag(0), ' '].concat(enumerals[0][1] ? [nameTagMembers(0), '\n'] : ['\n']);
    for (var i = 1; i < enumerals.length; i++) {
      declEnumerals = declEnumerals.concat(
        enumerals[i][1]
          ? ['  | ', nameTag(i), ' ', nameTagMembers(i), '\n']
          : ['  | ', nameTag(i), '\n']
      );
    }
    declEnumerals.push(['  deriving (P.Show, P.Eq)\n']);
    const decl = declName.concat(declEnumerals);

    // Data type declarations for members
    var declMemberDecls = [];
    for (var i = 0; i < enumerals.length; i++) {
      const members = enumerals[i][1];
      if (members == undefined) {
        continue;
      }
      // Data type declarations for member 
      const declMemberName = [
        '\n',
        'data ', nameTagMembers(i), ' = ', nameTagMembers(i), '\n'
      ];
      var declMembers = ['  { ', members[0][0], ' :: ', members[0][1], '\n'];
      for (var j = 1; j < members.length; j++) {
        declMembers = declMembers.concat(['  , ', members[j][0], ' :: ', members[j][1], '\n']);
      }
      var declDeriving = '  } deriving (P.Show, P.Eq, P.Generic)\n';
      const declMember = declMemberName.concat(declMembers).concat([declDeriving]);
      // ToJSON instance
      const toJSON  = [
        '\n',
        'instance A.ToJSON ', nameTagMembers(i), '\n'
      ];
      declMemberDecls = declMemberDecls.concat(declMember).concat(toJSON);
    }

    // ToJSON instance
    var toJSON  = [
      '\n',
      'instance A.ToJSON ', name, ' where\n',
      '  toJSON = \\case\n',
    ];
    for (var i = 0; i < enumerals.length; i++) {
      var line = [
        '    ', nameTag(i), ' ',
      ];
      const members = enumerals[i][1];
      if (members == undefined) {
        line = line.concat(['-> A.object [ "tag" A..= ("', enumerals[i][0], '" :: T.Text) ]\n']);
      } else {
        line = line.concat(['m -> C.combineObjects (A.object [ "tag" A..= ("', enumerals[i][0], '" :: T.Text) ]) (A.toJSON m)\n']);
      }
      toJSON = toJSON.concat(line);
    }

    // FromVal instance
    var fromVal = [
      '\n',
      'instance C.FromVal ', name, ' where\n',
      '  fromVal = \\case\n',
      '    C.Val\'ApiVal (C.ApiVal\'Enumerator (C.Enumerator tag m)) -> case (tag,m) of\n',
    ];
    for (var i = 0; i < enumerals.length; i++) {
      const members = enumerals[i][1];
      if (members == undefined) {
        fromVal = fromVal.concat([
          '      ("', enumerals[i][0], '", P.Nothing) -> P.Just ', nameTag(i), '\n',
        ]);
      } else {
        fromVal = fromVal.concat([
          '      ("', enumerals[i][0], '", P.Just m\') -> ', nameTag(i), ' P.<$> (', nameTagMembers(i), '\n',
        ]);
        fromVal = fromVal.concat([
          '          P.<$> C.getMember m\' "', members[0][0], '"\n'
        ]);
        for (var j = 1; j < members.length ; j++) {
          fromVal = fromVal.concat([
            '          P.<*> C.getMember m\' "', members[j][0], '"\n'
          ]);
        }
        fromVal = fromVal.concat([
          '        )\n',
        ]);
      }
    }
    fromVal = fromVal.concat([
      '    _ -> P.Nothing\n',
      '  _ -> P.Nothing\n',
    ]);


    // ToVal instance
    var toVal = [
      '\n',
      'instance C.ToVal ', name, ' where\n',
      '  toVal = \\case\n',
    ];
    for (var i = 0; i < enumerals.length; i++) {
      const members = enumerals[i][1];
      if (members == undefined) {
        toVal = toVal.concat([
          '    ', nameTag(i), ' -> C.Val\'ApiVal P.$ C.ApiVal\'Enumerator $ C.Enumerator "', enumerals[i][0], '" P.Nothing\n',
        ]);
      } else {
        toVal = toVal.concat([
          '    ', nameTag(i), ' ', nameTagMembers(i), '\n',
        ]);
        toVal = toVal.concat([
          '      { ', members[0][0], '\n'
        ]);
        for (var j = 1; j < members.length; j++) {
          toVal = toVal.concat([
            '      , ', members[j][0], '\n'
          ]);
        }
        toVal = toVal.concat([
          '      } -> C.Val\'ApiVal P.$ C.ApiVal\'Enumerator $ C.Enumerator "', enumerals[i][0], '" P.$ Map.fromList\n',
        ]);
        toVal = toVal.concat([
          '      [ ("', members[0][0], '", C.toVal ', members[0][0], ')\n'
        ]);
        for (var j = 1; j < members.length; j++) {
          toVal = toVal.concat([
            '      , ("', members[j][0], '", C.toVal ', members[j][0], ')\n'
          ]);
        }
        toVal = toVal.concat(['      ]\n']);
      }
    }

    return decl.concat(declMemberDecls).concat(toJSON).concat(fromVal).concat(toVal).join('');
  },


  api(name, calls) {
    const apiName = name + '\'Call';
    var api = [
      '\n',
      '-- API: ', name, '\n',
      'data ', apiName, '\n',
    ];
    api = api.concat([
      '  = ', apiName, '\'', calls[0][0],  calls[0][1] ? (' ' + calls[0][0]) : '', '\n',
    ]);
    for (var i = 1; i < calls.length; i++) {
      api = api.concat([
        '  | ', apiName, '\'', calls[i][0],  calls[i][1] ? (' ' + calls[i][0]) : '', '\n',
      ]);

    }
    var deriving = [
      '  deriving (P.Show, P.Eq)\n',
    ];
    return api.concat(deriving).join('');
  },


  serviceThrower(error) {
    return [
      '\n',
      '--\n',
      'class Monad m => ServiceThrower m where\n',
      '  serviceThrow :: ', error, ' -> m a\n',
    ].join('');
  },


  service(calls) {
    var lines = [
      '\n',
      '--\n',
      'class ServiceThrower m => Service meta m where\n'
    ];
    for (var i = 0; i < calls.length; i++) {
      lines = lines.concat([
        '  ', calls[i][0], ' :: meta ->', calls[i][1] ? (' ' + calls[i][1] + ' ->') : '', ' m ', calls[i][2], '\n',
      ]);
    }
    return lines.join('');
  },


  version(major,minor) {
    return [
      '\n',
      '--\n',
      'version :: C.Version\n',
      'version = C.Version ', major, ' ', minor, '\n',
    ].join('');
  },

  apiParser(name, calls) {
    var lines = [
      '\n',
      'apiParser :: C.ApiParser ', name, '\n',
      'apiParser = C.ApiParser\n',
    ];

    // Hollow
    if (calls.hollow.length > 0) {
      lines = lines.concat([
        '  { hollow = Map.fromList\n',
      ]);
      lines = lines.concat([
        '     [ ("', calls.hollow[0], '", ', name, '\'', calls.hollow[0], ')\n',
      ]);
      for (var i = 1; i < calls.hollow.length; i++) {
        lines = lines.concat([
          '     , ("', calls.hollow[i], '", ', name, '\'', calls.hollow[i], ')\n',
        ]);
      }
      lines = lines.concat([
        '     ]\n',
      ]);
    } else {
      lines = lines.concat([
        '  { hollow = Map.empty\n',
      ]);
    }

    // Struct
    if (calls.struct.length > 0) {
      lines = lines.concat([
        '  , struct = Map.fromList\n',
      ]);
      lines = lines.concat([
        '     [ ("', calls.struct[0], '", v ', name, '\'', calls.struct[0], ')\n',
      ]);
      for (var i = 1; i < calls.struct.length; i++) {
        lines = lines.concat([
          '     , ("', calls.struct[i], '", v ', name, '\'', calls.struct[i], ')\n',
        ]);
      }
      lines = lines.concat([
        '     ]\n',
      ]);
    } else {
      lines = lines.concat([
        '  , struct = Map.empty\n',
      ]);
    }

    // Enumeration
    if (calls.enumeration.length > 0) {
      lines = lines.concat([
        '  , enumeration = Map.fromList\n',
      ]);
      lines = lines.concat([
        '     [ ("', calls.enumeration[0], '", v ', name, '\'', calls.enumeration[0], ')\n',
      ]);
      for (var i = 1; i < calls.enumeration.length; i++) {
        lines = lines.concat([
          '     , ("', calls.enumeration[i], '", v ', name, '\'', calls.enumeration[i], ')\n',
        ]);
      }
      lines = lines.concat([
        '     ]\n',
      ]);
    } else {
      lines = lines.concat([
        '  , enumeration = Map.empty\n',
      ]);
    }
 
    // Wrap
    if (calls.wrap.length > 0) {
      lines = lines.concat([
        '  , wrap = Map.fromList\n',
      ]);
      lines = lines.concat([
        '     [ ("', calls.wrap[0], '", v ', name, '\'', calls.wrap[0], ')\n',
      ]);
      for (var i = 1; i < calls.wrap.length; i++) {
        lines = lines.concat([
          '     , ("', calls.wrap[i], '", v ', name, '\'', calls.wrap[i], ')\n',
        ]);
      }
      lines = lines.concat([
        '     ]\n',
      ]);
    } else {
      lines = lines.concat([
        '  , wrap = Map.empty\n',
      ]);
    }

    lines = lines.concat([
      '  }\n',
      '  where\n',
      '    v x y = x P.<$> C.fromVal y\n'
    ]);
 
    return lines.join('');
  },

  apiLookup(calls) {
    var lines = [
      '\n',
      '--\n',
      'api :: (Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val\n',
      'api meta apiCall = case C.parseApiCall apiParser apiCall of\n',
      '  P.Nothing -> C.runtimeThrow C.RuntimeError\'UnrecognizedCall\n',
      '  P.Just x -> case x of\n',
    ];
    for (var i = 0; i < calls.hollow.length; i++) {
      lines = lines.concat([
        '    Api\'', calls.hollow[i][0], ' -> C.toVal P.<$> ', calls.hollow[i][1], ' meta\n',
      ]);
    }
    for (var i = 0; i < calls.filled.length; i++) {
      lines = lines.concat([
        '    Api\'', calls.filled[i][0], ' a -> C.toVal P.<$> ', calls.filled[i][1], ' meta a\n',
      ]);
    }
    return lines.join('');
  },

  handleRequest(meta) {
    var lines = [
      '\n',
      '--\n',
      'handleRequest :: (Service meta m, C.RuntimeThrower m, IO.MonadIO m) => C.Options -> (', meta, ' -> m meta) -> C.Request -> m C.Response\n',
      'handleRequest options metaMiddleware C.Request{meta,calls} = do\n',
      '  meta\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableMeta) P.return (C.fromValFromJson meta)\n',
      '  xformMeta <- metaMiddleware meta\n',
      '  envRef <- IO.liftIO C.emptyEnv\n',
      '  variableBaseCount <- IO.liftIO (Map.size <$> IO.readIORef envRef)\n',
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
  },

  imports() {
    return [
      'import qualified Prelude as P\n',
      'import qualified Data.Map as Map\n',
      'import qualifeid Control.Monad.IO.Class as IO\n',
      'import qualified Data.Aeson as A\n',
      'import qualified Data.Text as T\n',
      'import qualified Data.Text.Conversions as T\n',
      'import qualified Data.Word as I\n',
      'import qualified Data.Int as I\n',
      'import qualified Data.IORef as IO\n',
      'import qualified GHC.Generics as P\n',
      'import qualified Colorless.Types as C\n',
      'import qualified Colorless.Runtime.Expr as C\n',
      'import qualified Colorless.Runtime.Val as C (ToVal(..), FromVal(..), getMember, fromValFromJson, combineObjects)\n',
    ].join('');
  },

  extensions() {
    return [
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
  },

  module(prefix, version, types) {
    var lines = [
      '\n',
      '-- Module\n',
      'module ', prefix, '.V', version.major, '_', version.minor, '\n',
      '  ( version\n',
      '  , handleRequest\n',
      '  , ServiceThrower(..)\n',
      '  , Service(..)\n',
      '  , Api(..)\n',
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
  }
};

function lowercaseFirst(str) {
  var s = str;
  s[0] = s[0].toLowerCase();
  return s;
}

console.log(haskell.wrap('Name',haskell.primMap['String']))
console.log(haskell.wrap('Email',haskell.primMap['String']))

console.log(haskell.struct('User', [
  { name: 'userId', label: 'userId', type: 'UUID'},
  { name: 'name', label: 'name', type: 'T.Text'},
  { name: 'email', label: 'email', type: 'Maybe Email'},
]));

console.log(haskell.enumeration('Color', [
  ['Red'],
  ['Blue'],
  ['Green'],
  ['Custom', [
      ['red','I.Word8'],['blue','I.Word8'],['green','I.Word8']]
    ]
])); 

console.log(haskell.api('Api', [
  ['Hello',true],
  ['GoodBye',false]
]));

console.log(haskell.serviceThrower('Error'));

console.log(haskell.service([
  ['hello', 'Hello', 'T.Text'],
  ['goodBye', null, '()']
]));

console.log(haskell.version(0,0))

console.log(haskell.apiParser('Api', {
  hollow: [
    'Hello'
  ],
  struct: [
    'Hola'
  ],
  enumeration: [
    'One',
    'Two',
    'Three',
  ],
  wrap: [
    'Goodbye',
  ],
}));

console.log(haskell.apiLookup({
  hollow: [
    ['Hello','hello'],
  ],
  filled: [
    ['Hola','hola'],
    ['One','one'],
    ['Two','two'],
    ['Three','three'],
    ['Four','four'],
  ],
}));

console.log(haskell.handleRequest('Meta'));

console.log(haskell.imports());

console.log(haskell.extensions());

console.log(haskell.module('MyModule.Somewhere', {major: 1, minor: 2}, [
  'Error',
  'Meta',
  'Hello',
  'Hola',
  'One',
  'Two',
  'Three',
]));
