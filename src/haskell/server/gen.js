const R = require('ramda');
var Lines = require('../../lines.js').Lines;

var {
  enumeralNameTagMember,
  mkExportTypes,
  mkImportTypes,
  genVersion,
  genWrap,
  genToJson,
  genFromJson,
  genWrapToVal,
  genWrapFromVal,
  genStruct,
  genStructToVal,
  genStructFromVal,
  genEnumeration,
  genEnumerationToVal,
  genEnumerationFromVal,
  isFunc,
  genPull,
} = require('../common.js');

const genApi = (name, calls) => {
  var lines = new Lines();
  lines.add([
    '\n',
    '-- Api\n',
    'data ', name, '\'Api\n',
  ]);
  lines.add([
    '  = ', name, '\'Api\'', calls[0].name,  calls[0].filled ? (' ' + calls[0].name) : '', '\n',
  ]);
  calls.slice(1).forEach(call =>
    lines.add([
      '  | ', name, '\'Api\'', call.name,  call.filled ? (' ' + call.name) : '', '\n'
    ])
  );
  lines.add([
    '  deriving (P.Show, P.Eq)\n',
  ]);
  return lines;
};


const genThrower = (name, lowercaseName, error) => {
  return new Lines([
    '\n',
    '-- Thrower\n',
    'class P.Monad m => ', name ,'\'Thrower m where\n',
    '  ', lowercaseName,'\'Throw :: ', error, ' -> m a\n',
  ]);
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

const genService = (name, calls) => {
  var lines = new Lines([
    '\n',
    '-- Service\n',
    'class ', name,'\'Thrower m => ', name ,'\'Service meta m where\n'
  ]);
  calls.forEach(call =>
    lines.add([
      '  ', call.func, ' :: meta ->', call.name ? (' ' + call.name + ' ->') : '', ' m ', call.output, '\n',
    ])
  );
  return lines;
};

const genApiParser = (name, lowercaseName, calls) => {
  var lines = new Lines([
    '\n',
    '-- API Parser\n',
    lowercaseName, '\'ApiParser :: C.ApiParser ', name, '\'Api\n',
    lowercaseName, '\'ApiParser = C.ApiParser\n',
  ]);

  // Hollow
  if (calls.hollow.length) {
    lines.add('  { hollow = Map.fromList\n');
    lines.add([
      '     [ ("', calls.hollow[0].label, '", ', name, '\'Api\'', calls.hollow[0].name, ')\n',
    ]);
    calls.hollow.slice(1).forEach(hollow =>
      lines.add([
        '     , ("', hollow.label, '", ', name, '\'Api\'', hollow.name, ')\n',
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
      '     [ ("', calls.struct[0].label, '", v ', name, '\'Api\'', calls.struct[0].name, ')\n',
    ]);
    calls.struct.slice(1).forEach(struct =>
      lines.add([
        '     , ("', struct.label, '", v ', name, '\'Api\'', struct.name, ')\n',
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
      '     [ ("', calls.enumeration[0].label, '", v ', name, '\'Api\'', calls.enumeration[0].name, ')\n',
    ]);
    calls.enumeration.slice(1).forEach(enumeration =>
      lines.add([
        '     , ("', enumeration.label, '", v ', name, '\'Api\'', enumeration.name, ')\n',
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
      '     [ ("', calls.wrap[0].label, '", v ', name, '\'Api\'', calls.wrap[0].name, ')\n',
    ]);
    calls.wrap.slice(1).forEach(wrap =>
      lines.add([
        '     , ("', wrap.label, '", v ', name, '\'Api\'', wrap.name, ')\n',
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

  return lines;
};

const genApiLookup = (name, lowercaseName, calls) => {
  var lines = new Lines([
    '\n',
    '-- API\n',
    lowercaseName, '\'ApiCall :: (', name, '\'Service meta m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val\n',
    lowercaseName, '\'ApiCall meta\' apiCall\' = case C.parseApiCall ', lowercaseName,'\'ApiParser apiCall\' of\n',
    '  P.Nothing -> C.runtimeThrow C.RuntimeError\'UnrecognizedCall\n',
    '  P.Just x\' -> case x\' of\n',
  ]);
  calls.hollow.forEach(hollow =>
    lines.add([
      '    ', name, '\'Api\'', hollow.name, ' -> C.toVal P.<$> ', hollow.func, ' meta\'\n',
    ])
  );
  calls.filled.forEach(filled =>
    lines.add([
      '    ', name, '\'Api\'', filled.name, ' a\' -> C.toVal P.<$> ', filled.func, ' meta\' a\'\n',
    ])
  );
  return lines;
};

const genHandleRequest = (name, lowercaseName, meta) => {
  return new Lines([
    '\n',
    '-- Handler\n',
    lowercaseName, '\'Handler :: (', name, '\'Service meta m, C.RuntimeThrower m, IO.MonadIO m) => C.Options -> (', meta, ' -> m meta) -> C.Request -> m C.Response\n',
    lowercaseName, '\'Handler options metaMiddleware C.Request{meta,query} = do\n',
    '  meta\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableMeta) P.return (C.fromValFromJson meta)\n',
    '  xformMeta <- metaMiddleware meta\'\n',
    '  envRef <- IO.liftIO C.emptyEnv\n',
    '  variableBaseCount <- IO.liftIO (Map.size P.<$> IO.readIORef envRef)\n',
    '  let options\' = C.Options\n',
    '        { variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit options)\n',
    '        }\n',
    '  let evalConfig = C.EvalConfig\n',
    '        { C.options = options\'\n',
    '        , C.apiCall = ', lowercaseName,'\'ApiCall xformMeta\n',
    '        }\n',
    '  query\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableQuery) P.return (C.jsonToExpr query)\n',
    '  vals <- C.runEval (C.forceVal P.=<< C.eval query\' envRef) evalConfig\n',
    '  P.return (C.Response\'Success (A.toJSON vals))\n',
  ]);
};

const genImports = (prefix, importTypes) => {
  var lines = new Lines([
    '\n',
    '-- Imports\n',
    'import qualified Prelude as P\n',
    'import qualified Control.Monad as P\n',
    'import qualified Data.Word as I\n',
    'import qualified Data.Int as I\n',
    'import qualified Data.IORef as IO\n',
    'import qualified Data.String as P (IsString)\n',
    'import qualified GHC.Generics as P (Generic)\n',

    'import qualified Control.Monad.IO.Class as IO\n',
    'import qualified Data.Aeson as A\n',
    'import qualified Data.Map as Map\n',
    'import qualified Data.Text as T\n',
    'import qualified Data.Text.Conversions as T\n',

    'import qualified Colorless.Server as C\n',
    '\n',
  ]);
  lines.add(importTypes.map(({ name, major }) =>
    'import ' + prefix + '.V' + major + ' (' + name + '(..))\n'
  ));
  return lines;
};

const genPragmas = () => {
  return new Lines([
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
  ]);
};

const genModule = (name, lowercaseName, prefix, version, types) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '.V', version.major, '\n',
    '  ( ', lowercaseName, '\'Version\n',
    '  , ', lowercaseName, '\'Pull\n',
    '  , ', lowercaseName,'\'Handler\n',
    '  , ', lowercaseName,'\'Spec\n',
    '  , ', name, '\'Thrower(..)\n',
    '  , ', name, '\'Service(..)\n',
  ]);
  types.forEach(type =>
    lines.add([
      '  , ', type, '(..)\n',
    ])
  );
  lines.add('  ) where\n');
  return lines;
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

const genSpec = ({lowercaseName, original}) => {
  var lines = new Lines();
  lines.add([
    '\n',
    lowercaseName, '\'Spec :: A.Value\n',
    lowercaseName, '\'Spec = v\n',
    '  where P.Just v = A.decode ', JSON.stringify(JSON.stringify(original)), '\n',
  ]);
  return lines;
};

const gen = (s) => {
  const exportTypes = mkExportTypes(s);
  const importTypes = mkImportTypes(s);
  const serviceCalls = mkServiceCalls(s);
  const apiLookupPairs = mkApiLookupPairs(s);
  const apiCalls = mkApiCalls(s);
  const apiParserCalls = mkApiParserCalls(s);

  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(s.name, s.lowercaseName, s.module, s.version, exportTypes));
  lines.add(genImports(s.module, importTypes));
  lines.add(genVersion(s.lowercaseName, s.version.major, s.version.minor));
  lines.add(genPull(s));
  lines.add(genThrower(s.name, s.lowercaseName, s.error));
  lines.add(genService(s.name, serviceCalls));
  lines.add(genHandleRequest(s.name, s.lowercaseName, s.meta));
  lines.add(genApiLookup(s.name, s.lowercaseName, apiLookupPairs));
  lines.add(genApiParser(s.name, s.lowercaseName, apiParserCalls));
  lines.add(genApi(s.name, apiCalls));

  s.wrap.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genWrap(ty));
    lines.add(genWrapToVal(ty));
    lines.add(genWrapFromVal(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
  });

  s.struct.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genStruct(ty));
    lines.add(genStructToVal(ty));
    lines.add(genStructFromVal(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
  });
  s.enumeration.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genEnumeration(ty));
    lines.add(genEnumerationToVal(ty));
    lines.add(genEnumerationFromVal(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
  });
  lines.add(genSpec(s));
  lines.add('\n');
  return lines.collapse();
};

module.exports.gen = gen;
