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
        evaId <- def "evaId" $ phonebook'InsertPerson $ insertPerson' $ InsertPerson $ Person "Eva" "123456789" Nothing []
        aliceId <- def "aliceId" $ phonebook'InsertPerson $ insertPerson'Mk <:>
          (person'Mk <:> name' "Alice" <:> phone' "8675309" <:> option Nothing <:> list [evaId])
        stmt $ tuple2 evaId aliceId


  putStrLn "\n\"Request\""
  printJSON req

  resp <- phonebook'HttpClient'SendRequest manager phonebook'pull [] req

  putStrLn "\n\"Response\""
  printJSON $ snd resp

printJSON :: ToJSON a => a -> IO ()
printJSON = BL.putStrLn . encode . toJSON
