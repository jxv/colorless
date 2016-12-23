module Colorless
  ( Console(..)
  , main
  , bannerMessage
  , ColorlessM
  , runIO
  , mainIO
  ) where

import Pregame

main :: Console m => m ()
main = writeLine bannerMessage

bannerMessage :: Text
bannerMessage = unlines
  [ ""
  , "  _ _ | _  _| _  _ _"
  , " (_(_)|(_)| |(/__\\_\\"
  , ""
  ]

class Monad m => Console m where
  writeLine :: Text -> m ()

newtype ColorlessM a = ColorlessM (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: ColorlessM a -> IO a
runIO (ColorlessM m) = m

mainIO :: IO ()
mainIO = runIO main

instance Console ColorlessM where
  writeLine = liftIO . putStrLn
