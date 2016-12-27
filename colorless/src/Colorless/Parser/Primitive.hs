{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser.Primitive
  ( primitiveType
  ) where

import Pregame
import Data.Tuple (uncurry)

import Colorless.Parser.Atomic
import Colorless.Parser.Types

primitiveType :: Atomic m => m PrimitiveType
primitiveType = match $ fmap (uncurry token)
  [ ("unit", PrimitiveTypeUnit)
  , ("u8", PrimitiveTypeU8)
  , ("u16", PrimitiveTypeU16)
  , ("u32", PrimitiveTypeU32)
  , ("u64", PrimitiveTypeU64)
  , ("i8", PrimitiveTypeI8)
  , ("i16", PrimitiveTypeI16)
  , ("i32", PrimitiveTypeI32)
  , ("i64", PrimitiveTypeI64)
  , ("f32", PrimitiveTypeF32)
  , ("f64", PrimitiveTypeF64)
  , ("bool", PrimitiveTypeBool)
  , ("char", PrimitiveTypeChar)
  , ("str", PrimitiveTypeStr)
  , ("int", PrimitiveTypeInt)
  , ("neg", PrimitiveTypeNeg)
  , ("pos", PrimitiveTypePos)
  , ("nat", PrimitiveTypeNat)
  , ("rat", PrimitiveTypeRat)
  ]
