module Main where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent.MVar
import Colorless.Types
import Colorless.Server.Scotty
import Data.Map (Map)

import Phonebook.V0

type DB = Map PersonId Person

newtype App a = App { unApp :: ReaderT (MVar DB) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MVar DB))

getDB :: App DB
getDB = ask >>= liftIO . readMVar

instance Phonebook'Service () App where
  phonebook'LookupPerson () LookupPerson{lookupPersonId = id'} = do
    db <- getDB
    return $ Map.lookup id' db
  phonebook'LookupPersonByName () LookupPersonByName{lookupPersonByNameName = name} = do
    db <- getDB
    return $ filter (\Person{personName} -> personName == name) $ map snd $ Map.toList db

main :: IO ()
main = do
  db <- newMVar Map.empty
  runServer phonebook'pull (\(App app) -> runReaderT app db) routes

routes :: ScottyT TL.Text App ()
routes = do
  phonebook'Scotty'Post defOptions return phonebook'pull
  phonebook'Scotty'Get phonebook'pull
