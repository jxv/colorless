module Colorless.Parser.Combinator
  ( Combinator(..)
  , choice'
  , assoc'
  , many'
  , some'
  , optional'
  ) where

import Pregame
import qualified Text.Megaparsec as P (string, choice, optional, some, many)

import Colorless.Parser.Types

class Monad m => Combinator m where
  choice :: NonEmpty (m a) -> m a
  assoc :: m a -> m a -> m a
  many :: m a -> m [a]
  some :: m a -> m (NonEmpty a)
  optional :: m a -> m (Maybe a)

choice' :: MonadParser m => NonEmpty (m a) -> m a
choice' (x :| xs) = P.choice (x : xs)

assoc' :: MonadParser m => m a -> m a -> m a
assoc' = (<|>)

many' :: MonadParser m => m a -> m [a]
many' = P.many

some' :: MonadParser m => m a -> m (NonEmpty a)
some' p = do
  x : xs <- P.some p
  return $ x :| xs

optional' :: MonadParser m => m a -> m (Maybe a)
optional' = P.optional
