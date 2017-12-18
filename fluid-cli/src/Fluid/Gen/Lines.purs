module Fluid.Gen.Lines where

import Prelude (Unit, pure, unit, discard, ($), (<>), flip)
import Data.Array as Array
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..))
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
addLine xs = line (Array.fold xs)

linesContent :: Lines Unit -> String
linesContent m = Array.intercalate "\n" (execWriter m)

lineList :: forall a. Array a -> String -> String -> (a -> Array String) -> Lines Unit
lineList arr headPrefix tailPrefix f = case Array.uncons arr of
  Nothing -> pure unit
  Just {head,tail} -> do
    addLine $ [headPrefix] <> f head
    flip traverse_ tail $ \item -> addLine $ [tailPrefix] <> f item
