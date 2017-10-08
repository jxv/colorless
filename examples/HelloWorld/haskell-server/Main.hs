{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Colorless.Types
import Colorless.Server.Scotty
import Data.Text.Lazy

import HelloWorld
import qualified HelloWorld.V0 as V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance HelloWorld'Service () App where
  hello _ _ = return "hi"

main :: IO ()
main = runServer helloWorld'Pull unApp routes

routes :: ScottyT Text App ()
routes = do
  V0.helloWorld'Scotty'SendResponse defOptions return helloWorld'Pull
  helloWorld'Scotty'GetSpec helloWorld'Pull
