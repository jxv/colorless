var Lines = require('../../../lines.js').Lines;

const importing = (s) => [
  'import qualified Colorless.Server.Scotty as Scotty',
];

const exporting = (s) => [
    s.lowercaseName + '\'Scotty\'Post',
    s.lowercaseName + '\'Scotty\'Get',
];

const gen = (s) => {
  return new Lines([
    '\n',
    s.lowercaseName, '\'Scotty\'Post\n',
    '  :: (Scotty.ScottyError e, R.MonadIO m, ', s.name,'\'Service meta m)\n',
    '  => C.Options\n',
    '  -> (', s.meta,' -> m meta)\n',
    '  -> C.Pull\n',
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'Post _options _metaMiddleware _pull = Scotty.sendResponseSingleton _pull ', s.lowercaseName,'\'version (', s.lowercaseName,'\'handler _options _metaMiddleware)\n',
    '\n',
    s.lowercaseName, '\'Scotty\'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'Get = Scotty.getSpec P.$ R.toJSON [', s.lowercaseName,'\'spec]\n',
  ]);
};

module.exports = {
  importing,
  exporting,
  gen,
};
