module Colorless.Syntax.Token
  ( Token(..)
  , primTyToken'
  , fnToken'
  , tyToken'
  , tagToken'
  , initiateModuleOverride'
  , moduleRefToken'
  , moduleVersionToken'
  , moduleVersionSeparator'
  ) where

import Colorless.Syntax.Atomic
import Colorless.Syntax.Combinator
import Colorless.Syntax.Types
import Data.Tuple (uncurry)
import Pregame

class Monad m => Token m where
  primTyToken :: m PrimTy
  opaqueTyRefToken :: m OpaqueTyRef
  opaqueTyToken :: m OpaqueTy
  fnToken :: m Fn
  tyToken :: m Ty
  tagToken :: m Tag
  initiateModuleOverride :: m ()
  moduleRefToken :: m ModuleRef
  moduleVersionToken :: m ModuleVersion
  moduleVersionSeparator :: m ()

primTyToken' :: (Atomic m, Combinator m) => m PrimTy
primTyToken' = choice $ fmap (uncurry token)
  [ ("unit", PrimTyUnit)
  , ("u8", PrimTyU8)
  , ("u16", PrimTyU16)
  , ("u32", PrimTyU32)
  , ("u64", PrimTyU64)
  , ("i8", PrimTyI8)
  , ("i16", PrimTyI16)
  , ("i32", PrimTyI32)
  , ("i64", PrimTyI64)
  , ("f32", PrimTyF32)
  , ("f64", PrimTyF64)
  , ("bool", PrimTyBool)
  , ("char", PrimTyChar)
  , ("str", PrimTyStr)
  , ("int", PrimTyInt)
  , ("neg", PrimTyNeg)
  , ("pos", PrimTyPos)
  , ("nat", PrimTyNat)
  , ("rat", PrimTyRat)
  ]

opaqueTyRefToken' :: (Atomic m, Token m) => m OpaqueTyRef
opaqueTyRefToken' = OpaqueTyRef <$> opaqueTyToken <*> pure []

fnToken' :: Atomic m => m Fn
fnToken' = Fn <$> lowerCamelCase

tyToken' :: (Atomic m, Token m, Combinator m) => m Ty
tyToken' = choice [TyPrim <$> primTyToken, TyOpaque <$> opaqueTyRefToken]

tagToken' :: Atomic m => m Tag
tagToken' = do
  match "#"
  Tag <$> upperCamelCase

initiateModuleOverride' :: Atomic m => m ()
initiateModuleOverride' = match "-"

moduleRefToken' :: Atomic m => m ModuleRef
moduleRefToken' = do
  match "%"
  ModuleRef <$> upperCamelCase

moduleVersionToken' :: Atomic m => m ModuleVersion
moduleVersionToken' = ModuleVersion <$> integer

moduleVersionSeparator' :: Atomic m => m ()
moduleVersionSeparator' = match "."
