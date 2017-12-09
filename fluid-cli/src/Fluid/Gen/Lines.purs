module Fluid.Gen.Lines where

import Prelude
import Data.Array (fold)
import Data.List (List(..), (:), fromFoldable)
import Data.Either (Either(..))
import Control.Monad.Writer
import Data.Traversable (traverse_)

type Lines a = Writer (List String) a

lines' :: List String -> Lines Unit
lines' lines = traverse_ (\x -> tell (x : Nil)) lines

lines :: Array String -> Lines Unit
lines xs = lines' (fromFoldable xs)

line :: String -> Lines Unit
line s = lines [s]

addLine :: Array String -> Lines Unit
addLine xs = line (fold xs)
