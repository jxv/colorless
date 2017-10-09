module Main where


import qualified Data.ByteString.Lazy.Char8 as BL (putStrLn)
import Network.HTTP.Client.TLS
import Data.Aeson (encode, ToJSON(..))
import Colorless.Client

import HelloWorld

main :: IO ()
main = do
  manager <- newTlsManager

  let helloBob = helloWorld'hello $ hello'Mk <:> string "Bob"
  let req = helloWorld'Request () helloBob

  putStrLn "\n\"Request\""
  printJSON req

  resp <- helloWorld'HttpClient'SendRequest manager helloWorld'Pull [] req

  putStrLn "\n\"Response\""
  printJSON $ snd resp

printJSON :: ToJSON a => a -> IO ()
printJSON = BL.putStrLn . encode . toJSON
