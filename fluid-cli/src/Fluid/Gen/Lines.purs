module Fluid.Gen.Lines where

import Prelude (Unit)
import Data.Array (fold, intercalate)
import Data.List (List(..), (:), fromFoldable)
import Control.Monad.Writer
import Data.Traversable (traverse_)

type Lines a = Writer (List String) a

lines' :: List String -> Lines Unit
lines' xs = traverse_ (\x -> tell (x : Nil)) xs

lines :: Array String -> Lines Unit
lines xs = lines' (fromFoldable xs)

line :: String -> Lines Unit
line s = lines [s]

addLine :: Array String -> Lines Unit
addLine xs = line (fold xs)

linesContent :: Lines Unit -> String
linesContent m = intercalate "\n" (execWriter m)
