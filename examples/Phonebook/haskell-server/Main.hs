module Main where

import Control.Monad.IO.Class
import Colorless.Types
import Colorless.Server.Scotty
import Data.Text.Lazy

import Phonebook.V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Phonebook'Service () App where
  lookupPerson () LookupPerson{} = return Nothing
  lookupPersonByName () LookupPersonByName{} = return []

main :: IO ()
main = runServer phonebook'Pull unApp routes

routes :: ScottyT Text App ()
routes = do
  phonebook'Scotty'SendResponse defOptions return phonebook'Pull
  phonebook'Scotty'GetSpec phonebook'Pull
