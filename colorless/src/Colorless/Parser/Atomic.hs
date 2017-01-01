module Colorless.Parser.Atomic
  ( Atomic(..)
  , token'
  , match'
  , literal'
  , newline'
  , char'
  , space'
  , satisfy'
  , integer'
  , upperCamelCase'
  ) where

import Pregame
import qualified Prelude (read)
import Data.Tuple (uncurry)
import Control.Applicative
import qualified Text.Megaparsec as P

import Colorless.Parser.Types

class Monad m => Atomic m where
  token :: Text -> a -> m a
  match :: Text -> m ()
  literal :: Text -> m Text
  newline :: m ()
  space :: m ()
  char :: Char -> m Char
  satisfy :: (Char -> Bool) -> m Char
  integer :: m Integer
  upperCamelCase :: m Text

token' :: MonadParser m => Text -> a -> m a
token' lexeme tkn = P.string (fromText lexeme) >> pure tkn

literal' :: MonadParser m => Text -> m Text
literal' lexeme = P.string (fromText lexeme) >> return lexeme

match' :: MonadParser m => Text -> m ()
match' lexeme = void $ P.string (fromText lexeme)

newline' :: MonadParser m => m ()
newline' = void P.newline

char' :: MonadParser m => Char -> m Char
char' = P.char

space' :: MonadParser m => m ()
space' = P.space

satisfy' :: MonadParser m => (Char -> Bool) -> m Char
satisfy' = P.satisfy

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

number' :: MonadParser m => m [Char]
number' = P.some P.digitChar

plus' :: MonadParser m => m [Char]
plus' = P.char '+' *> number'

minus' :: MonadParser m => m [Char]
minus' = P.char '-' <:> number'

integer' :: MonadParser m => m Integer
integer' = fmap Prelude.read $ plus' <|> minus' <|> number'

upperCamelCase' :: MonadParser m => m Text
upperCamelCase' = literal' "fail"
