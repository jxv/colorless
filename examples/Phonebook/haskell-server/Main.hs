module Main where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent.MVar
import Fluid.Types
import Fluid.Server
import Fluid.Server.Scotty
import Data.Map (Map)
import System.Random

import Phonebook.Server ()
import Phonebook.Major0

type DB = Map PersonId Person

newtype App a = App { unApp :: ReaderT (MVar DB) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (MVar DB), MonadCatch, MonadThrow)

getDB :: App DB
getDB = ask >>= liftIO . readMVar

modifyDB :: (DB -> DB) -> App ()
modifyDB f = do
  mdb <- ask
  db <- liftIO $ takeMVar mdb
  liftIO $ putMVar mdb (f db)

genId :: IO T.Text
genId = T.pack <$> liftM (take 10 . randomRs ('a','z')) newStdGen

instance Phonebook'Service () App where
  phonebook'InsertPerson () InsertPerson{insertPersonPerson = person} = do
    personId <- liftIO $ PersonId <$> genId
    modifyDB $ Map.insert personId person
    return personId
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
  phonebook'Scotty'Post (const defHooks) phonebook'pull
  phonebook'Scotty'Get phonebook'pull
