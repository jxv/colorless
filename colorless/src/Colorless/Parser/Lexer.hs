module Colorless.Parser.Lexer
  ( lex
  , lexer
  , runLexerM
  ) where

import Pregame
import Control.Applicative ((*>), (<*))
import Data.Either.Combinators (rightToMaybe)
import Colorless.Parser.Types
import qualified Text.Megaparsec as P
import Text.Megaparsec.Prim (MonadParsec)

newtype LexerM a = LexerM (P.ParsecT ParserError ParserState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus, MonadParsec ParserError ParserState)

runLexerM :: LexerM a -> Text -> IO (Either (P.ParseError (P.Token ParserState) ParserError) a)
runLexerM (LexerM m) input = P.runParserT m "" input

lexer :: Text -> IO (Maybe [LexToken])
lexer txt = rightToMaybe <$> runLexerM lexTokens txt

lexTokens :: MonadParser m => m [LexToken]
lexTokens = P.many (lexToken <* P.skipMany P.spaceChar)

lexToken :: MonadParser m => m LexToken
lexToken = lt
  where
    lt = LexToken <$> loc <*> lex

loc :: MonadParser m => m Loc
loc = do
  srcPos <- P.getPosition
  return $ Loc (line srcPos) (column srcPos)

line :: P.SourcePos -> Line
line = fromIntegral . P.unPos . P.sourceLine

column :: P.SourcePos -> Column
column = fromIntegral . P.unPos . P.sourceColumn

lex :: MonadParser m => m Lex
lex = choice
  [ LexLowerCamelCase <$> lowerCamelCase
  , P.char ':' *> pure LexColon
  ]

choice :: MonadParser m => NonEmpty (m a) -> m a
choice (x :| xs) = P.choice (x : xs)

lowerCamelCase :: MonadParser m => m Text
lowerCamelCase = do
  ch <- P.lowerChar
  chs <- P.many P.alphaNumChar
  return $ toText (ch : chs)

upperCamelCase :: MonadParser m => m Text
upperCamelCase = do
  ch <- P.upperChar
  chs <- P.many P.alphaNumChar
  return $ toText (ch : chs)
