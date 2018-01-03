module Fluid.Cli.Target where

import Prelude

import Control.Alt (alt)
import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile, FS, exists, mkdir)
import Node.Path (FilePath, dirname)

type Target =
  { path :: String
  , contents :: String
  }

writeTarget :: forall eff. Target -> Aff (fs :: FS | eff) Unit
writeTarget target = do
  let dirPath = dirname target.path
  exists' <- exists dirPath
  if exists'
    then pure unit
    else mkdir dirPath
  writeUTF8File target.path target.contents

writeUTF8File :: forall eff. FilePath -> String -> Aff (fs :: FS | eff) Unit
writeUTF8File = writeTextFile UTF8
