var Lines = require('../../../lines.js').Lines;

const importing = (s) => [
  'import qualified Colorless.Server.Scotty as Scotty',
];

const exporting = (s) => [
    s.lowercaseName + '\'Scotty\'Post',
    s.lowercaseName + '\'Scotty\'Get',
];

const gen = (specs) => {
  const s = specs[specs.length - 1];
  var lines = new Lines([
    '\n',
    s.lowercaseName, '\'Scotty\'Post\n',
    '  ::\n',
    '    ( Scotty.ScottyError e\n',
    '    , R.MonadIO m\n',
    '    , R.MonadCatch m\n'
  ]);
  lines.add(specs.map(({version}) =>
    '    , V' + version.major + '.' + s.name + '\'Service meta' + version.major + ' m\n'
  ));
  lines.add([
    '    )\n',
    '  => C.Pull\n',
  ]);
  specs.forEach(({meta, version}) => lines.add([
    '  -> C.Hooks m ', meta,' meta', version.major, '\n',
  ]));
  lines.add([
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'Post pull'
  ]);
  specs.forEach(({meta, version}) => lines.add([
    ' hooks', version.major,
  ]));
  lines.add([
    ' = Scotty.sendResponse pull (', s.lowercaseName, '\'handlerMap',
  ]);
  specs.forEach(({meta, version}) => lines.add([
    ' hooks', version.major,
  ]));
  lines.add([
    ')\n'
  ]);
  lines.add([
    '\n',
    s.lowercaseName, '\'Scotty\'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'Get = Scotty.getSpec ', s.lowercaseName,'\'spec\n',
  ]);
  return lines;
};

module.exports = {
  importing,
  exporting,
  gen,
};
