module Colorless.Parser.Atomic
  ( Atomic(..)
  , token'
  , literal'
  , match'
  ) where

import Pregame
import Data.Tuple (uncurry)
import Text.Megaparsec (string, ParsecT, runParserT, Token, Dec, ParseError, choice)
import Text.Megaparsec.Prim (MonadParsec)

import Colorless.Parser.Types

class Monad m => Atomic m where
  token :: Text -> a -> m a
  literal :: Text -> m Text
  match :: NonEmpty (m a) -> m a

token' :: MonadParsec ParserError ParserState m => Text -> a -> m a
token' lexme tkn = string (fromText lexme) >> pure tkn

literal' :: MonadParsec ParserError ParserState m => Text -> m Text
literal' lexme = string (fromText lexme) >> pure lexme

match' :: (MonadParsec ParserError ParserState m) => NonEmpty (m a) -> m a
match' (x :| xs) = choice (x : xs)
