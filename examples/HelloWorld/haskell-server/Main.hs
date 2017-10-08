module Main where

import Control.Monad.IO.Class
import Colorless.Types
import Colorless.Server.Scotty
import Data.Text.Lazy

import HelloWorld.V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance HelloWorld'Service () App where
  hello () Hello{target} = return $ "Hello, " `mappend` target

main :: IO ()
main = runServer helloWorld'Pull unApp routes

routes :: ScottyT Text App ()
routes = do
  helloWorld'Scotty'SendResponse defOptions return helloWorld'Pull
  helloWorld'Scotty'GetSpec helloWorld'Pull
