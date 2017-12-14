module Fluid.Cli.Target where

import Prelude
import Control.Monad.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile, FS)
import Node.Path (FilePath)

type Target =
  { path :: String
  , contents :: String
  }

writeTarget :: forall eff. Target -> Aff (fs :: FS | eff) Unit
writeTarget target = do
  writeUTF8File target.path target.contents

writeUTF8File :: forall eff. FilePath -> String -> Aff (fs :: FS | eff) Unit
writeUTF8File = writeTextFile UTF8
