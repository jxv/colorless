module Main where


import qualified Data.ByteString.Lazy.Char8 as BL (putStrLn)
import Network.HTTP.Client.TLS
import Data.Aeson (encode, ToJSON(..))
import Colorless.Client

import Phonebook

main :: IO ()
main = do
  manager <- newTlsManager

  let req = phonebook'request () $ begin $ do
        let eva = Person "Eva" "123456789" Nothing []
        evaId <- def "evaId" $ phonebook'InsertPerson $ ex (InsertPerson eva)
        aliceId <- def "aliceId" $ phonebook'InsertPerson $ insertPerson'Mk
          <:> (person'Mk <: "Alice" <: "8675309" <: Nothing <:> list [evaId])
        stmt $ tuple2 evaId aliceId


  putStrLn "\n\"Request\""
  printJSON req

  resp <- phonebook'HttpClient'Post manager phonebook'pull [] req

  putStrLn "\n\"Response\""
  printJSON $ snd resp

printJSON :: ToJSON a => a -> IO ()
printJSON = BL.putStrLn . encode . toJSON
