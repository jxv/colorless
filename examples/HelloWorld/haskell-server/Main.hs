module Main where

import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server
import Data.Text.Lazy

import HelloWorld ()
import HelloWorld.V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance HelloWorld'Thrower App

instance HelloWorld'Service () App where
  helloWorld'Hello () Hello{helloTarget} = return $ "Hello, " `mappend` helloTarget

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT Text App ()
routes = do
  helloWorld'Scotty'Post (const defHooks) helloWorld'pull
  helloWorld'Scotty'Get helloWorld'pull
