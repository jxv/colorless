module Main where

import Control.Monad.IO.Class
import Colorless.Types
import Colorless.Server.Scotty
import Colorless.Server
import Data.Text.Lazy

import HelloWorld ()
import HelloWorld.V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance HelloWorld'Thrower App

instance HelloWorld'Service () App where
  helloWorld'Hello () Hello{helloTarget} = do
    _ <- helloWorld'throw ()
    return $ "Hello, " `mappend` helloTarget

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT Text App ()
routes = do
  helloWorld'Scotty'Post defOptions return helloWorld'pull
  helloWorld'Scotty'Get helloWorld'pull
