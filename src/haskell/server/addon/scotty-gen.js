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
    '  :: (Scotty.ScottyError e, R.MonadIO m, ', s.name,'\'Service meta m, R.MonadCatch m)\n',
    '  => C.Hooks m ', s.meta, ' meta\n',
    '  -> C.Pull\n',
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'Post _hooks _pull = Scotty.sendResponseSingleton _pull ', s.lowercaseName,'\'version (', s.lowercaseName,'\'handler _hooks)\n',
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
