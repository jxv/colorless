module Fluid.Gen.Haskell.Common where

import Data.Maybe

primitiveMap :: String -> Maybe String
primitiveMap str = case str of
  "Unit" -> Just "()"
  "Bool" -> Just "P.Bool"
  "Int" -> Just "P.Int"
  "Float" -> Just "P.Float"
  "Char" -> Just "P.Char"
  "String" -> Just "P.Text"
  _ -> Nothing
