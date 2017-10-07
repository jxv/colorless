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
    '  :: (Scotty.ScottyError e, R.MonadIO m, C.RuntimeThrower m, ', s.name,'\'Service meta m)\n',
    '  -> C.Options\n',
    '  -> Meta\'Middlewares m',
  ]);
  lines.add(specs.map(({version}) =>
    ' meta' + version.major
  ));
  lines.add([
    '\n',
    '  -> C.Pull\n',
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'SendResponse options metaMiddlewares pull = ScottyT.sendResponse pull ', s.lowercaseName,'\'Version (handler\'Map options metaMiddlewares)\n',
    '\n',
    s.lowercaseName, '\'Scotty\'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'GetSpec = ScottyT.getSpec ', s.lowercaseName,'\'PublicSpec\n',
  ]);
  return lines;
};

module.exports = {
  importing,
  exporting,
  gen,
};
