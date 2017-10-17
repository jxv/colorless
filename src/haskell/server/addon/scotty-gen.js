var Lines = require('../../../lines.js').Lines;

const importing = (s) => [
  'import qualified Fluid.Server.Scotty as Scotty',
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
    '  => ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m ', s.meta, ' meta)\n',
    '  -> C.Pull\n',
    '  -> Scotty.ScottyT e m ()\n',
    s.lowercaseName, '\'Scotty\'Post _hooks _pull = Scotty.respondSingleton _pull ', s.lowercaseName,'\'version (\\_xtra -> ', s.lowercaseName,'\'handler _hooks _xtra)\n',
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
