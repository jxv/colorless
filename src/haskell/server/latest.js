var Lines = require('../../lines.js').Lines;
var {
  mkExportTypes,
  genPragmas,
} = require('../common.js');

const genModule = (prefix, name, lowercaseName, major, exportTypes) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '\n',
    '  ( handler\'Map\n',
    '  , Meta\'Middlewares(..)\n',
  ]);
  lines.add([
    '  , V', major, '.', name,'\'Service(..)\n',
    '  , V', major, '.', name,'\'Thrower(..)\n',
    '  , V', major, '.', lowercaseName,'\'Pull\n',
  ]);
  lines.add(exportTypes.map(type => '  , V' + major  + '.' + type + '(..)\n'))
  lines.add('  ) where\n');
  return lines;
};

const genCommonImports = () => {
  return new Lines([
    '\n',
    'import qualified Data.Map as Map\n',
    'import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor)\n',
    'import qualified Control.Monad.IO.Class as M (MonadIO)\n',
  ]);
};

const genVersionImports = (prefix, name, lowercaseName, major, exportTypes) => {
  var lines = new Lines([
    '\n',
    'import qualified ', prefix, '.V', major, ' as V', major, '\n',
  ]);
  lines.add([
    '  ( ', name,'\'Service(..)\n',
    '  , ', name,'\'Thrower(..)\n',
    '  , ', lowercaseName, '\'Handler\n',
    '  , ', lowercaseName, '\'Version\n',
    '  , ', lowercaseName, '\'Pull\n',
  ]);
  lines.add(exportTypes.map(type => '  , ' + type + '(..)\n'));
  lines.add('  )\n');
  return lines;
};

const genMetaMiddlewares = (specs) => {
  var lines = new Lines([
    '\n',
    'data Meta\'Middlewares m',
  ]);
  lines.add(specs.map(({version}) =>
    ' meta' + version.major
  ))
  lines.add([
    '\n',
    '  = Meta\'Middlewares\n',
  ]);
  lines.add(['  { meta\'Middleware', specs[0].version.major ,' :: ', specs[0].metaVersion, ' -> m meta', specs[0].version.major,'\n']);
  specs.slice(1).forEach(spec =>
    lines.add(['  , meta\'Middleware', spec.version.major ,' :: ', spec.metaVersion, ' -> m meta', spec.version.major,'\n'])
  );
  lines.add('  }\n');
  return lines;
}

const genHandlerMap = (specs) => {
  var lines = new Lines();
  lines.add([
    '\n',
    'handler\'Map\n',
    '  ::\n',
    '    ( M.MonadIO m\n',
    '    , C.RuntimeThrower m\n',
  ]);
  lines.add(specs.map(spec =>
    '    , V' + spec.version.major + '.' +  spec.name + '\'Service meta' + spec.version.major + ' m\n',
  ));
  lines.add([
    '    )\n',
    '  => C.Options\n',
    '  -> Meta\'Middlewares m',
  ]);
  lines.add(specs.map(({version}) =>
    ' meta' + version.major
  ));
  lines.add([
    '\n',
    '  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)\n',
    'handler\'Map options metaMiddlewares = Map.fromList\n',
  ]);

  lines.add(
    '    [ (' + specs[0].version.major + ', (' + specs[0].version.minor + ', V' + specs[0].version.major + '.' + specs[0].lowercaseName + '\'Handler options $ meta\'Middleware' + specs[0].version.major + ' metaMiddlewares))\n'
  );
  lines.add(specs.slice(1).map(spec =>
    '    , (' + spec.version.major + ', (' + spec.version.minor + ', V' + spec.version.major + '.' + spec.lowercaseName + '\'Handler options $ meta\'Middleware' + spec.version.major + ' metaMiddlewares))\n'
  ));
  lines.add(
    '    ]\n'
  );
  return lines;
}

const latest = (specs) => {
  const spec = specs[specs.length - 1];
  const exportTypes = mkExportTypes(spec);

  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(spec.module, spec.name, spec.lowercaseName, spec.version.major, exportTypes));
  lines.add(genCommonImports());
  specs.forEach(spec =>
    lines.add(genVersionImports(spec.module, spec.name, spec.lowercaseName, spec.version.major, mkExportTypes(spec)))
  );
  lines.add(genMetaMiddlewares(specs));
  lines.add(genHandlerMap(specs));
  lines.add('\n');
  return lines.collapse();
};

module.exports = {
  latest: latest,
};
