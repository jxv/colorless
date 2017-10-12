const R = require('ramda');
var Lines = require('../../lines.js').Lines;
var scotty = require('./addon/scotty-gen.js');

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

const genImports = (prefix, importTypes, importing) => {
  var lines = new Lines([
    '\n',
    '-- Imports\n',
    'import qualified Prelude as P\n',
    'import qualified Control.Monad as P\n',
    'import qualified Control.Monad.Except as M\n',
    'import qualified Data.Word as I\n',
    'import qualified Data.Int as I\n',
    'import qualified Data.IORef as IO\n',
    'import qualified Data.String as P (IsString)\n',
    '\n',
    'import qualified Colorless.Imports as R\n',
    'import qualified Colorless.Server as C\n',
    '\n',
  ]);
  lines.add(importTypes.map(({ name, major }) =>
    'import ' + prefix + '.V' + major + ' (' + name + '(..))\n'
  ));
  lines.add('\n');
  importing.forEach(x => lines.add(x + '\n'));
  return lines;
};

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
  var lines = new Lines([
    '\n',
    '-- Thrower\n',
    'class C.ServiceThrower m => ', name ,'\'Thrower m where\n',
    '  ', lowercaseName,'\'throw :: ', error, ' -> m a\n',
    '  ', lowercaseName,'\'throw = C.serviceThrow P.. R.toJSON P.. C.toVal\n',
  ]);
  return lines;
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

const genService = (name, lowercaseName, calls) => {
  var lines = new Lines([
    '\n',
    '-- Service\n',
    'class P.Monad m => ', name ,'\'Service meta m where\n'
  ]);
  calls.forEach(call =>
    lines.add([
      '  ', lowercaseName, '\'', call.name, ' :: meta ->', call.name ? (' ' + call.name + ' ->') : '', ' m ', call.output, '\n',
    ])
  );

  lines.add([
    '\n',
    'instance ', name,'\'Service meta m => ', name ,'\'Service meta (M.ExceptT C.Response m) where\n'
  ]);
  calls.forEach(call =>
    lines.add([
      '  ', lowercaseName, '\'', call.name, ' _meta = M.lift ', call.name ? ' P.. ' : ' P.$ ', lowercaseName, '\'', call.name, ' _meta\n',
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
    lines.add('  { hollow = R.fromList\n');
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
    lines.add('  { hollow = R.empty\n');
  }

  // Struct
  if (calls.struct.length) {
    lines.add('  , struct = R.fromList\n');
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
    lines.add('  , struct = R.empty\n');
  }

  // Enumeration
  if (calls.enumeration.length) {
    lines.add('  , enumeration = R.fromList\n');
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
    lines.add('  , enumeration = R.empty\n');
  }

  // Wrap
  if (calls.wrap.length) {
    lines.add('  , wrap = R.fromList\n');
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
    lines.add('  , wrap = R.empty\n');
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
    lowercaseName, '\'ApiCall :: (', name, '\'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val\n',
    lowercaseName, '\'ApiCall meta\' apiCall\' = case C.parseApiCall ', lowercaseName,'\'ApiParser apiCall\' of\n',
    '  P.Nothing -> C.runtimeThrow C.RuntimeError\'UnrecognizedCall\n',
    '  P.Just x\' -> case x\' of\n',
  ]);
  calls.hollow.forEach(hollow =>
    lines.add([
      '    ', name, '\'Api\'', hollow.name, ' -> C.toVal P.<$> ', lowercaseName, '\'', hollow.name, ' meta\'\n',
    ])
  );
  calls.filled.forEach(filled =>
    lines.add([
      '    ', name, '\'Api\'', filled.name, ' a\' -> C.toVal P.<$> ', lowercaseName, '\'', filled.name, ' meta\' a\'\n',
    ])
  );
  return lines;
};

const genHandleRequest = (name, lowercaseName, meta) => {
  return new Lines([
    '\n',
    '-- Handler\n',
    lowercaseName, '\'handler\n',
    '  :: (', name, '\'Service meta m, R.MonadIO m, R.MonadCatch m)\n',
    '  => C.Options\n',
    '  -> (', meta, ' -> m meta)\n',
    '  -> C.Request\n',
    '  -> m (P.Either C.Response C.Response)\n',
    lowercaseName, '\'handler _options metaMiddleware C.Request{meta,query} = R.catch\n',
    '  (M.runExceptT P.$ do\n',
    '    meta\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableMeta) P.return (C.fromValFromJson meta)\n',
    '    xformMeta <- M.lift P.$ metaMiddleware meta\'\n',
    '    envRef <- R.liftIO C.emptyEnv\n',
    '    variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)\n',
    '    let _limits\' = (C.hardLimits _options)\n',
    '          { C.variableLimit = P.fmap (P.+ variableBaseCount) (C.variableLimit P.$ C.hardLimits _options)\n',
    '          }\n',
    '    _serviceCallCountRef <- R.liftIO (IO.newIORef 0)\n',
    '    _lambdaCountRef <- R.liftIO (IO.newIORef 0)\n',
    '    _exprCountRef <- R.liftIO (IO.newIORef 0)\n',
    '    let evalConfig = C.EvalConfig\n',
    '          { C.limits = _limits\'\n',
    '          , C.langServiceCallCount = _serviceCallCountRef\n',
    '          , C.langLambdaCount = _lambdaCountRef\n',
    '          , C.langExprCount = _exprCountRef\n',
    '          , C.apiCall = ', lowercaseName,'\'ApiCall xformMeta\n',
    '          }\n',
    '    query\' <- P.maybe (C.runtimeThrow C.RuntimeError\'UnparsableQuery) P.return (C.jsonToExpr query)\n',
    '    vals <- C.runEval (C.forceVal P.=<< C.eval query\' envRef) evalConfig\n',
    '    P.return (C.Response\'Success (R.toJSON vals)))\n',
    '  (\\_err -> P.return P.$ P.Left (C.Response\'Error (C.ResponseError\'Service _err)))\n'
  ]);
};


const genModule = (name, lowercaseName, prefix, version, types, values) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '.V', version.major, '\n',
    '  ( ', lowercaseName, '\'version\n',
    '  , ', lowercaseName, '\'pull\n',
    '  , ', lowercaseName,'\'handler\n',
    '  , ', lowercaseName,'\'spec\n',
    '  , ', name, '\'Thrower(..)\n',
    '  , ', name, '\'Service(..)\n',
  ]);
  types.forEach(type =>
    lines.add([
      '  , ', type, '(..)\n',
    ])
  );
  values.forEach(value =>
    lines.add([
      '  , ', value,'\n',
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
    lowercaseName, '\'spec :: R.Value\n',
    lowercaseName, '\'spec = v\n',
    '  where P.Just v = R.decode ', JSON.stringify(JSON.stringify(original)), '\n',
  ]);
  return lines;
};

const gen = (s, addons) => {
  const exportTypes = mkExportTypes(s);
  const importTypes = mkImportTypes(s);
  const serviceCalls = mkServiceCalls(s);
  const apiLookupPairs = mkApiLookupPairs(s);
  const apiCalls = mkApiCalls(s);
  const apiParserCalls = mkApiParserCalls(s);

  const addonOptions = { 'scotty': scotty };
  var addonImporting = [];
  var addonExporting = [];
  var addonGen = [];
  addons.forEach(addon => {
    var option = addonOptions[addon];
    if (option) {
      addonImporting = addonImporting.concat(option.importing(s));
      addonExporting = addonExporting.concat(option.exporting(s));
      addonGen.push(option.gen(s));
    }
  });

  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(s.name, s.lowercaseName, s.module, s.version, exportTypes, addonExporting));
  lines.add(genImports(s.module, importTypes, addonImporting));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Configs\n');
  lines.add('--------------------------------------------------------\n');

  lines.add(genVersion(s.lowercaseName, s.version.major, s.version.minor));
  lines.add(genPull(s));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Interfaces\n');
  lines.add('--------------------------------------------------------\n');

  lines.add(genThrower(s.name, s.lowercaseName, s.error));
  lines.add(genService(s.name, s.lowercaseName, serviceCalls));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Types\n');
  lines.add('--------------------------------------------------------\n');
  s.wrap.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genWrap(ty));
  });
  s.struct.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genStruct(ty));
  });
  s.enumeration.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genEnumeration(ty));
  });

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Add-ons\n');
  lines.add('--------------------------------------------------------\n');
  addonGen.forEach(gen => lines.add(gen));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Request handling\n');
  lines.add('--------------------------------------------------------\n');
  lines.add(genHandleRequest(s.name, s.lowercaseName, s.meta));
  lines.add(genApiLookup(s.name, s.lowercaseName, apiLookupPairs));
  lines.add(genApiParser(s.name, s.lowercaseName, apiParserCalls));
  lines.add(genApi(s.name, apiCalls));

  lines.add('\n');
  lines.add('--------------------------------------------------------\n');
  lines.add('-- Type Instances\n');
  lines.add('--------------------------------------------------------\n');

  s.wrap.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genWrapToVal(ty));
    lines.add(genWrapFromVal(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
  });

  s.struct.filter(currentTypeSource(s)).forEach(ty => {
    lines.add(genStructToVal(ty));
    lines.add(genStructFromVal(ty));
    lines.add(genToJson(ty));
    lines.add(genFromJson(ty));
  });
  s.enumeration.filter(currentTypeSource(s)).forEach(ty => {
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
