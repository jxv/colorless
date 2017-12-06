module Fluid.Gen.Spec where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Traversable (traverse)
import Simple.JSON (readJSON, class ReadForeign, readImpl)
import Data.Foreign (F, Foreign, readString, readArray, fail, ForeignError(..))
import Data.Foreign.Index ((!), hasProperty)
import Data.StrMap as StrMap
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.Array as Array

type Version =
  { major :: Int
  , minor :: Int
  }

data Type = Type
  { n :: String
  , p :: Maybe (Array Type)
  }

instance eqType :: Eq Type where
  eq (Type a) (Type b) = a.n == b.n && a.p == b.p

instance readForeignType :: ReadForeign Type where
  readImpl = readType

readType :: Foreign -> F Type
readType value = readTypeString value <|> readTypeObject value

readTypeString :: Foreign -> F Type
readTypeString value = do
  n <- readString value
  pure $ Type { n: n, p: Nothing }

readTypeObject :: Foreign -> F Type
readTypeObject value = do
  n <- value ! "n" >>= readString
  if hasProperty "p" value
    then do
      p <- value ! "p" >>= (\v -> readTypeParam v <|> readTypeParams v)
      pure $ Type { n: n, p: Just p }
    else pure $ Type { n: n, p: Nothing }

readTypeParam :: Foreign -> F (Array Type)
readTypeParam value = do
  param <- readType value
  pure [param]

readTypeParams :: Foreign -> F (Array Type)
readTypeParams value = do
  array <- readArray value
  traverse readType array

type Pull =
  { protocol :: String
  , name :: String
  , host :: String
  , path :: String
  , port :: Int
  , error :: Type
  }

type Schema = StrMap TypeDecl

type Spec =
  { fluid :: Version
  , version :: Maybe Version
  , pull :: Pull
  , schema :: Schema
  }

data TypeDecl
  = TypeDecl'Hollow { o :: Type }
  | TypeDecl'Wrap { w :: Type, o :: Maybe Type }
  | TypeDecl'Enum { e :: Array EnumDecl, o :: Maybe Type }
  | TypeDecl'Struct { m :: Array MemberDecl, o :: Maybe Type }

instance readForeignTypeDecl :: ReadForeign TypeDecl where
  readImpl = readTypeDecl

instance eqTypeDecl :: Eq TypeDecl where
  eq (TypeDecl'Hollow h) (TypeDecl'Hollow h') = h.o == h'.o
  eq (TypeDecl'Wrap w) (TypeDecl'Wrap w') = w.w == w'.w && w.o == w'.o
  eq (TypeDecl'Enum e) (TypeDecl'Enum e') = e.e == e'.e && e.o == e'.o
  eq (TypeDecl'Struct s) (TypeDecl'Struct s') = s.m == s'.m && s.o == s'.o
  eq _ _ = false

readTypeDecl :: Foreign -> F TypeDecl
readTypeDecl value =
  readTypeDeclHollow value <|>
  readTypeDeclWrap value <|>
  readTypeDeclEnum value <|>
  readTypeDeclStruct value

readTypeDeclHollow :: Foreign -> F TypeDecl
readTypeDeclHollow value = do
  o <- value ! "o" >>= readImpl
  pure $ TypeDecl'Hollow { o: o }

readMaybeOutput :: Foreign -> F (Maybe Type)
readMaybeOutput value = do
  if hasProperty "o" value
    then do
      o <- value ! "o" >>= readImpl
      pure (Just o)
    else pure Nothing

readTypeDeclWrap :: Foreign -> F TypeDecl
readTypeDeclWrap value = readTypeDeclWrapString value <|> readTypeDeclWrapObject value

readTypeDeclWrapString :: Foreign -> F TypeDecl
readTypeDeclWrapString value = do
  w <- readTypeString value
  pure $ TypeDecl'Wrap { w: w, o: Nothing }

readTypeDeclWrapObject :: Foreign -> F TypeDecl
readTypeDeclWrapObject value = do
  w <- value ! "w" >>= readTypeString
  o <- readMaybeOutput value
  pure $ TypeDecl'Wrap { w: w, o: o }

readTypeDeclEnum :: Foreign -> F TypeDecl
readTypeDeclEnum value =
  readTypeDeclEnumArray value <|>
  readTypeDeclEnumObject value

readTypeDeclEnumArray :: Foreign -> F TypeDecl
readTypeDeclEnumArray value = do
  e <- readArray value
  e' <- traverse readImpl e
  pure $ TypeDecl'Enum { e: e', o: Nothing }

readTypeDeclEnumObject :: Foreign -> F TypeDecl
readTypeDeclEnumObject value = do
  e <- value ! "e" >>= readArray
  e' <- traverse readImpl e
  o <- readMaybeOutput value
  pure $ TypeDecl'Enum { e: e', o: o }

readTypeDeclStruct :: Foreign -> F TypeDecl
readTypeDeclStruct value = do
  m <- value ! "m" >>= readArray
  m' <- traverse readImpl m
  o <- readMaybeOutput value
  pure $ TypeDecl'Struct { m: m', o: o }

data EnumDecl = EnumDecl
  { tag :: String
  , m :: Maybe (Array MemberDecl)
  }

instance eqEnumDecl :: Eq EnumDecl where
  eq (EnumDecl a) (EnumDecl b) = a.tag == b.tag && a.m == b.m

instance readForeignEnumDecl :: ReadForeign EnumDecl where
  readImpl = readEnumDecl

readEnumDecl :: Foreign -> F EnumDecl
readEnumDecl value = readEnumDeclString value <|> readEnumDeclObject value

readEnumDeclString :: Foreign -> F EnumDecl
readEnumDeclString value = do
  tag <- readString value
  pure $ EnumDecl { tag: tag, m: Nothing }

readEnumDeclObject :: Foreign -> F EnumDecl
readEnumDeclObject value = do
  tag <- value ! "tag" >>= readString
  m <- if hasProperty "m" value
         then do
           m <- value ! "m" >>= readArray
           m' <- traverse readImpl m
           pure $ if Array.null m' then Nothing else Just m'
         else pure Nothing
  pure $ EnumDecl { tag: tag, m: m }

data MemberDecl = MemberDecl
  { name :: String
  , ty :: Type
  }

instance eqMemberDecl :: Eq MemberDecl where
  eq (MemberDecl a) (MemberDecl b) = a.name == b.name && a.ty == b.ty

instance readForeignMemberDecl :: ReadForeign MemberDecl where
  readImpl = readMemberDecl

readMemberDecl :: Foreign -> F MemberDecl
readMemberDecl value = do
  strMap <- readImpl value
  case StrMap.toUnfoldable strMap of
    [] -> fail (ForeignError "No object")
    [Tuple name ty] -> pure $ MemberDecl { name: name, ty: ty }
    _ -> fail (ForeignError "Must be singleton object")

parseSpec :: String -> Either String Spec
parseSpec s = lmap show $ readJSON s
