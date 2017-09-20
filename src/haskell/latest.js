var Lines = require('../lines.js').Lines;
var { mkExportTypes } = require('./common.js');

const genPragmas = () => {
  return new Lines([
    '-- Pragmas\n',
    '{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n'
  ]);
};

const genModule = (prefix, exportTypes) => {
  var lines = new Lines([
    '\n',
    '-- Module\n',
    'module ', prefix, '\n',
    '  ( mkHandleRequestMap\n',
    '  , MetaMiddlewares(..)\n',
    '  , handleRequest_V0\n',
  ]);
  lines.add([
    '  , V0.Service(..)\n',
    '  , V0.ServiceThrower(..)\n',
  ]);
  lines.add(exportTypes.map(type => '  , V0.' + type + '(..)\n'))
  lines.add('  ) where\n');
  return lines;
};

const genImports = (prefix, exportTypes) => {
  var lines = new Lines([
    '\n',
    'import qualified Data.Map as Map\n',
    'import qualified Colorless.Types as C (RuntimeThrower, Options, Request, Response, Major, Minor)\n',
    'import qualified Control.Monad.IO.Class as M (MonadIO)\n',
  ]);
  lines.add([
    '\n',
    'import ', prefix, '.V0 as V0\n',
  ]);
  lines.add([
    '  ( Service(..)\n',
    '  , ServiceThrower(..)\n',
    '  , handleRequest\n',
    '  , version\n'
  ]);
  lines.add(exportTypes.map(type => '  , ' + type + '(..)\n'));
  lines.add('  )\n');
  return lines;
};

const genHandleRequest = (meta) => {
  var lines = new Lines([
    '\n',
    'handleRequest_V0 :: (V0.Service meta m, C.RuntimeThrower m, M.MonadIO m) => C.Options -> (', meta ,' -> m meta) -> C.Request -> m C.Response\n',
    'handleRequest_V0 = V0.handleRequest\n'
  ]);
  return lines;
}

const genMetaMiddlewares = (meta) => {
  var lines = new Lines([
    '\n',
    'data MetaMiddlewares\n',
    '    m\n',
    '    meta_V0\n',
    '  = MetaMiddlewares\n',
  ]);
  lines.add(['  { metaMiddleware_V0 :: ', meta, ' -> m meta_V0\n']);
  lines.add('  }\n');
  return lines;
}

const genMkHandleRequestMap = () => {
  var lines = new Lines();
  lines.add([
    '\n',
    'mkHandleRequestMap\n',
    '  ::\n',
    '    ( M.MonadIO m\n',
    '    , C.RuntimeThrower m\n',
    '    , V0.Service meta_V0 m\n',
    '    )\n',
    '  => C.Options\n',
    '  -> MetaMiddlewares\n',
    '      m\n',
    '      meta_V0\n',
    '  -> Map.Map C.Major (C.Minor, C.Request -> m C.Response)\n',
    'mkHandleRequestMap options metaMiddlewares = Map.fromList\n',
    '    [ (0, (0, V0.handleRequest options $ metaMiddleware_V0 metaMiddlewares))\n',
    '    ]\n'
  ]);
  return lines;
}

const latest = (spec) => {
  const exportTypes = mkExportTypes(spec);
  var lines = new Lines();
  lines.add(genPragmas());
  lines.add(genModule(spec.module, exportTypes));
  lines.add(genImports(spec.module, exportTypes));
  lines.add(genHandleRequest(spec.meta));
  lines.add(genMetaMiddlewares(spec.meta));
  lines.add(genMkHandleRequestMap());
  return lines.collapse();
};

module.exports = {
  latest: latest,
};
