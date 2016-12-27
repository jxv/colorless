{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser
  ( ParserM
  , runParserM
  ) where

import Pregame
import Text.Megaparsec (string, ParsecT, runParserT, Token, Dec, ParseError, choice)
import Text.Megaparsec.Prim (MonadParsec)

import Colorless.Parser.Atomic
import Colorless.Parser.Types

newtype ParserM a = ParserM (ParsecT ParserError ParserState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus, MonadParsec ParserError ParserState)

runParserM :: ParserM a -> Text -> IO (Either (ParseError (Token ParserState) ParserError) a)
runParserM (ParserM m) input = runParserT m "" input

instance Atomic ParserM where
  literal = literal'
  token = token'
  match = match'
