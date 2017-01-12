module Colorless.CodeGen.Target.Haskell.Client
  ( fnSignature
  , fnDefVal
  , tcDeclFn
  ) where

import Pregame
import Data.List (intersperse)
import Colorless.Semantic.Types

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

primRef :: PrimRef -> Text
primRef = \case
  PrimRefUnit -> "()"
  PrimRefU8 x -> showText x
  PrimRefU16 x -> showText x
  PrimRefU32 x -> showText x
  PrimRefU64 x -> showText x
  PrimRefI8 x -> showText x
  PrimRefI16 x -> showText x
  PrimRefI32 x -> showText x
  PrimRefI64 x -> showText x
  PrimRefF32 x -> showText x
  PrimRefF64 x -> showText x
  PrimRefBool x -> showText x
  PrimRefChar x -> showText x
  PrimRefStr x -> showText x
  PrimRefInt x -> showText x
  PrimRefNeg (Neg x) -> showText x
  PrimRefPos (Pos x) -> showText x
  PrimRefNat (Nat x) -> showText x
  PrimRefRat (Rat x) -> showText x

opaqueMonoRef :: OpaqueMonoRef -> Level -> Text
opaqueMonoRef OpaqueMonoRef{ _name, _params } = \case
  TopLevel -> out
  Nested -> parensate _params out
  where
    preParamSpace = if null _params then "" else " "
    out = toText _name <> preParamSpace <> mconcat (intersperse " " (map (\param -> monoTyParamRef param Nested) _params))

monoTyRef :: MonoTyRef -> Level -> Text
monoTyRef (MonoTyRefPrimTy x) = const (primTy x)
monoTyRef (MonoTyRefOpaque x) = opaqueMonoRef x

monoTyParamRef :: MonoTyParamRef -> Level -> Text
monoTyParamRef (MonoTyParamRefPrimRef x) = const (primRef x)
monoTyParamRef (MonoTyParamRefPrimTy x) = const (primTy x)
monoTyParamRef (MonoTyParamRefOpaqueRef x) = opaqueMonoRef x

fnSignature :: FnDef -> Text
fnSignature FnDef{ _args, _output } = mconcat $ intersperse " -> " $ map (flip monoTyRef TopLevel . snd) _args ++ ["m " <> monoTyRef _output Nested]

tcDeclFn :: Fn -> FnDef -> Text
tcDeclFn fn fnDef = "  " <> toText fn <> " :: " <> fnSignature fnDef

fnDefVal :: FnDef
fnDefVal = FnDef
  [ ("a", MonoTyRefPrimTy PrimTyInt)
  , ("b", MonoTyRefOpaque (OpaqueMonoRef "Either" [MonoTyParamRefPrimTy PrimTyInt, MonoTyParamRefOpaqueRef (OpaqueMonoRef "Color" [])]))
  ]
  (MonoTyRefOpaque $ OpaqueMonoRef "Either" [MonoTyParamRefPrimTy PrimTyStr, MonoTyParamRefOpaqueRef (OpaqueMonoRef "Color" [])])
  mempty
