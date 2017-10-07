var Lines = require('../../../lines.js').Lines;

const importing = (s) => [
  'import qualified Colorless.Client.HttpClient as HttpClient',
];

const exporting = (s) => [
    s.lowercaseName + '\'HttpClient\'SendRequest',
];

const gen = (s) => {
  return new Lines([
    '\n',
    s.lowercaseName, '\'HttpClient\'SendRequest\n',
    '  :: (c.HasType a, C.ToAst a, R.FromJSON a)\n',
    '  => HttpClient.Manager\n',
    '  -> C.Pull\n',
    '  -> HttpClient.RequestHeaders\n',
    '  -> C.Request ', s.meta,' a\n',
    '  -> P.IO (HttpClient.HttpClientResponse C.ByteString, P.Maybe (C.Response ', s.error,' a))\n',
    s.lowercaseName, '\'HttpClient\'SendRequest = C.sendRequest\n',
  ]);
};

module.exports = {
  importing,
  exporting,
  gen,
};
