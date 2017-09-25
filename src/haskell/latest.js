var Lines = require('../lines.js').Lines;
var { mkExportTypes } = require('./common.js');

const genPragmas = () => {
  return new Lines([
    '-- Pragmas\n',
    '{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n'
  ]);
};

const genModule = (prefix, major, exportTypes) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '\n',
    '  ( mkHandleRequestMap\n',
    '  , MetaMiddlewares(..)\n',
  ]);
  lines.add([
    '  , V', major, '.Service(..)\n',
    '  , V', major, '.ServiceThrower(..)\n',
  ]);
  lines.add(exportTypes.map(type => '  , V' + major  + '.' + type + '(..)\n'))
  lines.add('  ) where\n');
  return lines;
};

const genCommonImports = () => {
  return new Lines([
    '\n',
    'import qualified Data.Map as Map\n',
    'import qualified Colorless.Types as C (RuntimeThrower, Options, Request, Response, Major, Minor)\n',
    'import qualified Control.Monad.IO.Class as M (MonadIO)\n',
  ]);
};

const genVersionImports = (prefix, major, exportTypes) => {
  var lines = new Lines([
    '\n',
    'import qualified ', prefix, '.V', major, ' as V', major, '\n',
  ]);
  lines.add([
    '  ( Service(..)\n',
    '  , ServiceThrower(..)\n',
    '  , handleRequest\n',
    '  , version\n'
  ]);
  lines.add(exportTypes.map(type => '  , ' + type + '(..)\n'));
  lines.add('  )\n');
  return lines;
};

const genMetaMiddlewares = (specs) => {
  var lines = new Lines([
    '\n',
    'data MetaMiddlewares m',
  ]);
  lines.add(specs.map(({version}) =>
    ' meta' + version.major
  ))
  lines.add([
    '\n',
    '  = MetaMiddlewares\n',
  ]);
  lines.add(['  { metaMiddleware', specs[0].version.major ,' :: ', specs[0].metaVersion, ' -> m meta', specs[0].version.major,'\n']);
  specs.slice(1).forEach(spec =>
    lines.add(['  , metaMiddleware', spec.version.major ,' :: ', spec.metaVersion, ' -> m meta', spec.version.major,'\n'])
  );
  lines.add('  }\n');
  return lines;
}

const genMkHandleRequestMap = (versions) => {
  var lines = new Lines();
  lines.add([
    '\n',
    'mkHandleRequestMap\n',
    '  ::\n',
    '    ( M.MonadIO m\n',
    '    , C.RuntimeThrower m\n',
  ]);
  lines.add(versions.map(version =>
    '    , V' + version.major + '.Service meta' + version.major + ' m\n',
  ));
  lines.add([
    '    )\n',
    '  => C.Options\n',
    '  -> MetaMiddlewares m',
  ]);
  lines.add(versions.map(version =>
    ' meta' + version.major
  ));
  lines.add([
    '\n',
    '  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)\n',
    'mkHandleRequestMap options metaMiddlewares = Map.fromList\n',
  ]);

  lines.add(
    '    [ (' + versions[0].major + ', (' + versions[0].minor + ', V' + versions[0].major + '.handleRequest options $ metaMiddleware' + versions[0].major + ' metaMiddlewares))\n'
  );
  lines.add(versions.slice(1).map(version =>
    '    , (' + version.major + ', (' + version.minor + ', V' + version.major + '.handleRequest options $ metaMiddleware' + version.major + ' metaMiddlewares))\n'
  ));
  lines.add(
    '    ]\n'
  );
  return lines;
}

const latest = (specs) => {
  const spec = specs[specs.length - 1];
  const exportTypes = mkExportTypes(spec);
  const versions = specs.map(s => s.version);

  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(spec.module, spec.version.major, exportTypes));
  lines.add(genCommonImports());
  specs.forEach(spec =>
    lines.add(genVersionImports(spec.module, spec.version.major, mkExportTypes(spec)))
  );
  lines.add(genMetaMiddlewares(specs));
  lines.add(genMkHandleRequestMap(versions));
  lines.add('\n');
  return lines.collapse();
};

module.exports = {
  latest: latest,
};
