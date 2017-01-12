module Colorless.Syntax.Atomic
  ( Atomic(..)
  , literal'
  , match'
  , token'
  , eol'
  , space'
  , char'
  , satisfy'
  , integer'
  , lowerCamelCase'
  , upperCamelCase'
  , parens'
  ) where

import Pregame
import qualified Prelude (read)
import Data.Tuple (uncurry)
import Control.Applicative
import qualified Text.Megaparsec as P

import Colorless.Syntax.Types

class Monad m => Atomic m where
  token :: Text -> a -> m a
  match :: Text -> m ()
  literal :: Text -> m Text
  eol :: m ()
  space :: m ()
  char :: Char -> m ()
  satisfy :: (Char -> Bool) -> m Char
  integer :: m Integer
  lowerCamelCase :: m Text
  upperCamelCase :: m Text
  parens :: m a -> m a

token' :: MonadParser m => Text -> a -> m a
token' lexeme tkn = P.string (fromText lexeme) >> pure tkn

literal' :: MonadParser m => Text -> m Text
literal' lexeme = P.string (fromText lexeme) >> return lexeme

match' :: MonadParser m => Text -> m ()
match' lexeme = void $ P.string (fromText lexeme)

eol' :: MonadParser m => m ()
eol' = void P.eol

char' :: MonadParser m => Char -> m ()
char' = void . P.char

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

lowerCamelCase' :: MonadParser m => m Text
lowerCamelCase' = do
  ch <- P.lowerChar
  chs <- P.many P.alphaNumChar
  return $ toText (ch : chs)

upperCamelCase' :: MonadParser m => m Text
upperCamelCase' = do
  ch <- P.upperChar
  chs <- P.many P.alphaNumChar
  return $ toText (ch : chs)

parens' :: MonadParser m => m a -> m a
parens' = P.between (P.char '(') (P.char ')')
