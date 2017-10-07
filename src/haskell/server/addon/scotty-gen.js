var Lines = require('../../../lines.js').Lines;

const importing = (s) => [
  'import qualified Colorless.Server.Scotty as Scotty',
];

const exporting = (s) => [
    s.lowercaseName + '\'Scotty\'SendResponse',
    s.lowercaseName + '\'Scotty\'GetSpec',
];

const gen = (s) => {
  return new Lines([
    '\n',
    s.lowercaseName, '\'Scotty\'SendResponse\n',
    '  :: (Scotty.ScottyError e, R.MonadIO m, C.RuntimeThrower m, ', s.name,'\'Service meta m)\n',
    '  => C.Options\n',
    '  -> (', s.meta,' -> m meta)\n',
    '  -> C.Pull\n',
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'SendResponse _options _metaMiddleware _pull = ScottyT.sendResponseSingleton _pull ', s.lowercaseName,'\'Version (', s.lowercaseName,'\'Handler _options _metaMiddleware)\n',
    '\n',
    s.lowercaseName, '\'Scotty\'GetSpec :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'GetSpec = ScottyT.getSpec P.$ R.toJSON [', s.lowercaseName,'\'Spec]\n',
  ]);
};

module.exports = {
  importing,
  exporting,
  gen,
};
