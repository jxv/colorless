var Lines = require('../../../lines.js').Lines;

const importing = (s) => [
  'import qualified Colorless.Server.Scotty as Scotty',
];

const exporting = (s) => [
    s.lowercaseName + '\'Scotty\'SendResponse',
    s.lowercaseName + '\'Scotty\'GetSpec',
];

const gen = (specs) => {
  const s = specs[specs.length - 1];
  var lines = new Lines([
    '\n',
    s.lowercaseName, '\'Scotty\'SendResponse\n',
    '  ::\n',
    '    ( Scotty.ScottyError e\n',
    '    , R.MonadIO m\n',
  ]);
  lines.add(specs.map(({version}) =>
    '    , V' + version.major + '.' + s.name + '\'Service meta' + version.major + ' m\n'
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
    '  -> C.Pull\n',
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'SendResponse options metaMiddlewares pull = Scotty.sendResponse pull (handler\'Map options metaMiddlewares)\n',
    '\n',
    s.lowercaseName, '\'Scotty\'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'GetSpec = Scotty.getSpec handler\'PublicSpec\n',
  ]);
  return lines;
};

module.exports = {
  importing,
  exporting,
  gen,
};
