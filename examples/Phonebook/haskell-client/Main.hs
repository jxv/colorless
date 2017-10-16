module Main where

import qualified Data.ByteString.Lazy.Char8 as BL (putStrLn)
import Network.HTTP.Client.TLS
import Data.Aeson (encode, ToJSON(..))
import Colorless.Client

import Phonebook

main :: IO ()
main = do
  manager <- newTlsManager

  let req = phonebook'request () $ dO $ do
        r <- def "r" $ reduceList -< (fn2 "x" "y" addI32, i32 1, ex [10])
        cmp <- defn "cmp" $ fn1 "x" $ \x -> eq x (i8 10)
        f <- defn "f" $ fn1 "x" $ \x -> addI8 x (i8 8)
        oleOneTwo <- def "oleOneTwo" $ filterList -< (cmp, mapList -< (f, ex[1, 2]))
        eva <- def "eva" $ ex $ Person "Eva" "123456789" Nothing []
        evaId <- def "evaId" $ phonebook'InsertPerson $ insertPerson'Mk <:> eva
        aliceId <- def "aliceId" $ phonebook'InsertPerson $ insertPerson'Mk
          <:> (person'Mk <: "Alice" <: "8675309" <: Nothing <:> list [evaId])

        val <- def "str" $ state'Match (ex $ State'Other $ State'Other'Members "HI")
          (string "cali")
          (string "new york")
          (string "tex")
          ("state", \c -> dO $ do
            name <- def "name" $ get state'Other'name c
            stmt $ name `concaT` name `concaT` name `concaT` name)
        stmt $ tuple5 evaId aliceId oleOneTwo r val

  putStrLn "\n\"Request\""
  printJSON req

  resp <- phonebook'HttpClient'Post manager phonebook'pull [] req

  putStrLn "\n\"Response\""
  printJSON $ snd resp

printJSON :: ToJSON a => a -> IO ()
printJSON = BL.putStrLn . encode . toJSON
