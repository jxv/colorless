module Colorless.Syntax.Monad
  ( SyntaxM
  , runSyntaxM
  ) where

import Pregame
import Text.Megaparsec (string, ParsecT, runParserT, Token, Dec, ParseError, choice)
import Text.Megaparsec.Prim (MonadParsec)

import Colorless.Syntax.Atomic
import Colorless.Syntax.Combinator
import Colorless.Syntax.Types

newtype SyntaxM a = SyntaxM (ParsecT ParserError ParserState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus, MonadParsec ParserError ParserState)

runSyntaxM :: SyntaxM a -> Text -> IO (Either (ParseError (Token ParserState) ParserError) a)
runSyntaxM (SyntaxM m) input = runParserT m "" input

instance Atomic SyntaxM where
  literal = literal'
  token = token'
  match = match'
  eol = eol'
  char = char'
  satisfy = satisfy'
  space = space'
  integer = integer'
  lowerCamelCase = lowerCamelCase'
  upperCamelCase = upperCamelCase'
  parens = parens'

instance Combinator SyntaxM where
  choice = choice'
  assoc = assoc'
  many = many'
  some = some'
  optional = optional'
