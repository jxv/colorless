module Colorless.CodeGen.Target.Haskell.ClientHttpJson
  ( fnSignature
  , fnDefVal
  , tcDeclFn
  ) where

import Pregame
import Data.List (intersperse)
import Colorless.Semantic.Types

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty

showText :: Show a => a -> Text
showText = toText . show

data Level
   = TopLevel
   | Nested
  deriving (Show, Eq)

parens :: Text -> Text
parens x = "(" <> x <> ")"

parensate :: [a] -> Text -> Text
parensate xs y = if null xs then y else parens y

primTy :: PrimTy -> Text
primTy = \case
  PrimTyUnit -> "()"
  PrimTyU8 -> "Word8"
  PrimTyU16 -> "Word16"
  PrimTyU32 -> "Word32"
  PrimTyU64 -> "Word64"
  PrimTyI8 -> "Int8"
  PrimTyI16 -> "Int16"
  PrimTyI32 -> "Int32"
  PrimTyI64 -> "Int64"
  PrimTyF32 -> "Float"
  PrimTyF64 -> "Double"
  PrimTyBool -> "Bool"
  PrimTyChar -> "Char"
  PrimTyStr -> "Text"
  PrimTyInt -> "Integer"
  PrimTyNeg -> "Neg"
  PrimTyPos -> "Pos"
  PrimTyNat -> "Nat"
  PrimTyRat -> "Rat"

primDeref :: PrimDeref -> Text
primDeref = \case
  PrimDerefUnit -> "()"
  PrimDerefU8 x -> showText x
  PrimDerefU16 x -> showText x
  PrimDerefU32 x -> showText x
  PrimDerefU64 x -> showText x
  PrimDerefI8 x -> showText x
  PrimDerefI16 x -> showText x
  PrimDerefI32 x -> showText x
  PrimDerefI64 x -> showText x
  PrimDerefF32 x -> showText x
  PrimDerefF64 x -> showText x
  PrimDerefBool x -> showText x
  PrimDerefChar x -> showText x
  PrimDerefStr x -> showText x
  PrimDerefInt x -> showText x
  PrimDerefNeg (Neg x) -> showText x
  PrimDerefPos (Pos x) -> showText x
  PrimDerefNat (Nat x) -> showText x
  PrimDerefRat (Rat x) -> showText x

opaqueMonoDeref :: OpaqueMonoDeref -> Level -> Text
opaqueMonoDeref OpaqueMonoDeref{ _name, _params } = \case
  TopLevel -> out
  Nested -> parensate _params out
  where
    preParamSpace = if null _params then "" else " "
    out = toText _name <> preParamSpace <> mconcat (intersperse " " (map (\param -> monoTyParamDeref param Nested) _params))

monoTyDeref :: MonoTyDeref -> Level -> Text
monoTyDeref (MonoTyDerefPrimTy x) = const (primTy x)
monoTyDeref (MonoTyDerefOpaque x) = opaqueMonoDeref x

monoTyParamDeref :: MonoTyParamDeref -> Level -> Text
monoTyParamDeref (MonoTyParamDerefPrimDeref x) = const (primDeref x)
monoTyParamDeref (MonoTyParamDerefPrimTy x) = const (primTy x)
monoTyParamDeref (MonoTyParamDerefOpaqueDeref x) = opaqueMonoDeref x

fnSignature :: FnDef -> Text
fnSignature FnDef{ _args, _output } = mconcat $ intersperse " -> " $ map (flip monoTyDeref TopLevel . snd) _args ++ ["m " <> monoTyDeref _output Nested]

tcDeclFn :: Fn -> FnDef -> Text
tcDeclFn fn fnDef = "  " <> toText fn <> " :: " <> fnSignature fnDef

fnDefVal :: FnDef
fnDefVal = FnDef
  [ ("a", MonoTyDerefPrimTy PrimTyInt)
  , ("b", MonoTyDerefOpaque (OpaqueMonoDeref "Either" [MonoTyParamDerefPrimTy PrimTyInt, MonoTyParamDerefOpaqueDeref (OpaqueMonoDeref "Color" [])]))
  ]
  (MonoTyDerefOpaque $ OpaqueMonoDeref "Either" [MonoTyParamDerefPrimTy PrimTyStr, MonoTyParamDerefOpaqueDeref (OpaqueMonoDeref "Color" [])])
  mempty
