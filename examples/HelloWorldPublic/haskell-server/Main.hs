module Main where

import Control.Monad.IO.Class
import Colorless.Types
import Colorless.Server.Scotty
import Data.Text.Lazy

import HelloWorld.V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance HelloWorld'Service () App where
  helloWorld'Hello () Hello{helloTarget} = return $ "Hello, " `mappend` helloTarget

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT Text App ()
routes = do
  helloWorld'Scotty'Post defOptions return helloWorld'pull
  helloWorld'Scotty'Get helloWorld'pull
