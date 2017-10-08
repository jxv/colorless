var Lines = require('../../lines.js').Lines;
var scotty = require('./addon/scotty-latest.js');

var {
  mkExportTypes,
  genPragmas,
} = require('../common.js');

const genModule = (prefix, name, lowercaseName, major, exportTypes, values) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '\n',
    '  ( handler\'Map\n',
    '  , handler\'PublicSpec\n',
    '  , Meta\'Middlewares(..)\n',
  ]);
  values.forEach(value => lines.add('  , ' + value + '\n'));
  lines.add([
    '  , V', major, '.', name,'\'Service(..)\n',
    '  , V', major, '.', name,'\'Thrower(..)\n',
    '  , V', major, '.', lowercaseName,'\'Pull\n',
  ]);
  lines.add(exportTypes.map(type => '  , V' + major  + '.' + type + '(..)\n'))
  lines.add('  ) where\n');
  return lines;
};

const genImports = (importing) => {
  var lines = new Lines([
    '\n',
    'import qualified Prelude as P\n',
    'import qualified Colorless.Server as C (RuntimeThrower, Options, Request, Response, Major, Minor, Pull)\n',
    'import qualified Colorless.Imports as R\n'
  ]);
  importing.forEach(imp => lines.add(imp + '\n'));
  return lines;
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
    '  , ', lowercaseName, '\'Spec\n',
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
    '    ( R.MonadIO m\n',
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
    '  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))\n',
    'handler\'Map options metaMiddlewares = R.fromList\n',
  ]);

  lines.add(
    '    [ (' + specs[0].version.major + ', (' + specs[0].version.minor + ', V' + specs[0].version.major + '.' + specs[0].lowercaseName + '\'Handler options P.$ meta\'Middleware' + specs[0].version.major + ' metaMiddlewares))\n'
  );
  lines.add(specs.slice(1).map(spec =>
    '    , (' + spec.version.major + ', (' + spec.version.minor + ', V' + spec.version.major + '.' + spec.lowercaseName + '\'Handler options P.$ meta\'Middleware' + spec.version.major + ' metaMiddlewares))\n'
  ));
  lines.add(
    '    ]\n'
  );
  return lines;
}

const genPublicSpec = (lowercaseName, specs) => {
  var lines = new Lines([
    '\n',
    'handler\'PublicSpec :: R.Value\n',
    'handler\'PublicSpec = R.toJSON\n',
    '  [ V', specs[0].version.major, '\.', specs[0].lowercaseName, '\'Spec\n',
  ]);
  specs.slice(1).forEach(spec =>
    lines.add(['  , V', spec.version.major,  '\.', specs[0].lowercaseName, '\'Spec\n'])
  );
  lines.add('  ]\n');
  return lines;
};

const latest = (specs, addons) => {
  const spec = specs[specs.length - 1];
  const exportTypes = mkExportTypes(spec);

  const addonOptions = { 'scotty': scotty };
  var addonImporting = [];
  var addonExporting = [];
  var addonGen = [];
  addons.forEach(addon => {
    var option = addonOptions[addon];
    if (option) {
      addonImporting = addonImporting.concat(option.importing(spec));
      addonExporting = addonExporting.concat(option.exporting(spec));
      addonGen.push(option.gen(specs));
    }
  });

  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(spec.module, spec.name, spec.lowercaseName, spec.version.major, exportTypes, addonExporting));
  lines.add(genImports(addonImporting));
  specs.forEach(spec =>
    lines.add(genVersionImports(spec.module, spec.name, spec.lowercaseName, spec.version.major, mkExportTypes(spec)))
  );
  lines.add(genMetaMiddlewares(specs));
  lines.add(genHandlerMap(specs));
  lines.add(genPublicSpec(spec.lowercaseName, specs))
  addonGen.forEach(gen => lines.add(gen));
  lines.add('\n');
  return lines.collapse();
};

module.exports = {
  latest: latest,
};
