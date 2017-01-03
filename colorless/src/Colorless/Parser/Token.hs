{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser.Token
  ( Token(..)
  , primitiveTypeToken'
  , functionToken'
  , typeToken'
  , tagToken'
  , initiateModuleOverride'
  , moduleReferenceToken'
  , moduleVersionToken'
  , moduleVersionSeparator'
  ) where

import Colorless.Parser.Atomic
import Colorless.Parser.Combinator
import Colorless.Parser.Types
import Data.Tuple (uncurry)
import Pregame

class Monad m => Token m where
  primitiveTypeToken :: m PrimitiveType
  opaqueTypeReferenceToken :: m OpaqueTypeReference
  opaqueTypeToken :: m OpaqueType
  functionToken :: m Function
  typeToken :: m Type
  tagToken :: m Tag
  initiateModuleOverride :: m ()
  moduleReferenceToken :: m ModuleReference
  moduleVersionToken :: m ModuleVersion
  moduleVersionSeparator :: m ()

primitiveTypeToken' :: (Atomic m, Combinator m) => m PrimitiveType
primitiveTypeToken' = choice $ fmap (uncurry token)
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

opaqueTypeReferenceToken' :: (Atomic m, Token m) => m OpaqueTypeReference
opaqueTypeReferenceToken' = OpaqueTypeReference <$> opaqueTypeToken <*> pure []

functionToken' :: Atomic m => m Function
functionToken' = Function <$> lowerCamelCase

typeToken' :: (Atomic m, Token m, Combinator m) => m Type
typeToken' = choice [TypePrimitive <$> primitiveTypeToken, TypeOpaque <$> opaqueTypeReferenceToken]

tagToken' :: Atomic m => m Tag
tagToken' = do
  match "#"
  Tag <$> upperCamelCase

initiateModuleOverride' :: Atomic m => m ()
initiateModuleOverride' = match "-"

moduleReferenceToken' :: Atomic m => m ModuleReference
moduleReferenceToken' = do
  match "%"
  ModuleReference <$> upperCamelCase

moduleVersionToken' :: Atomic m => m ModuleVersion
moduleVersionToken' = ModuleVersion <$> integer

moduleVersionSeparator' :: Atomic m => m ()
moduleVersionSeparator' = match "."
