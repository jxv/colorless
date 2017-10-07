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
    s.lowercaseName, '\'Scotty\'SendResponse :: (Scotty.ScottyError e, R.MonadIO m, C.RuntimeThrower m, ', s.name,'\'Service meta m) => C.Pull -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'SendResponse _pull = ScottyT.sendResponseSingleton _pull ', s.lowercaseName,'\'Version ', s.lowercaseName,'\'Handler\n',
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
