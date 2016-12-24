module Colorless.Parser.Types
  ( PrimitiveType(..)
  , OpaqueType(..)
  , Type(..)
  , Tag(..)
  , Function(..)
  , Label(..)
  , SumTag(..)
  , TagDeclaration(..)
  , FunctionDeclaration(..)
  , SumDeclaration(..)
  , ProductDeclaration(..)
  , Declaration(..)
  , ParserError
  , ParserState
  ) where

import Pregame
import Data.Tuple (uncurry)
import Text.Megaparsec (string, ParsecT, runParserT, Token, Dec, ParseError, choice)
import Text.Megaparsec.Prim (MonadParsec)

data PrimitiveType
  = PrimitiveTypeUnit
  | PrimitiveTypeU8
  | PrimitiveTypeU16
  | PrimitiveTypeU32
  | PrimitiveTypeU64
  | PrimitiveTypeI8
  | PrimitiveTypeI16
  | PrimitiveTypeI32
  | PrimitiveTypeI64
  | PrimitiveTypeF32
  | PrimitiveTypeF64
  | PrimitiveTypeBool
  | PrimitiveTypeChar
  | PrimitiveTypeStr
  | PrimitiveTypeInt
  | PrimitiveTypeNeg
  | PrimitiveTypePos
  | PrimitiveTypeNat
  | PrimitiveTypeRat
  deriving (Show, Eq)

newtype OpaqueType = OpaqueType Text
  deriving (Show, Eq, Ord, IsString, ToText)

data Type
  = TypePrimitive PrimitiveType
  | TypeOpaque OpaqueType
  deriving (Show, Eq)

newtype Tag = Tag Text
  deriving (Show, Eq, IsString, ToText)

newtype Function = Function Text
  deriving (Show, Eq, IsString, ToText)

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype SumTag = SumTag Text
  deriving (Show, Eq, Ord, IsString, ToText)

data TagDeclaration = TagDeclaration
  { _tag :: Tag
  } deriving (Show, Eq)

data FunctionDeclaration = FunctionDeclaration
  { _function :: Function
  , _parameters :: [(Maybe Label, Type)]
  , _output :: Type
  , _tag :: Tag
  } deriving (Show, Eq)

data SumDeclaration = SumDeclaration
  { _type :: OpaqueType
  , _parameters :: [(Label, Maybe PrimitiveType)]
  , _subtypes :: Map SumTag [Type]
  , _tags :: [Tag]
  } deriving (Show, Eq)

data ProductDeclaration = ProductDeclaration
  { _type :: OpaqueType
  , _parameters :: [(Label, Maybe PrimitiveType)]
  , _labels :: Map Label Type
  , _tags :: [Tag]
  } deriving (Show, Eq)

data Declaration
  = DeclarationTag TagDeclaration
  | DeclarationFunction FunctionDeclaration
  | DeclarationSum SumDeclaration
  | DeclarationProduct ProductDeclaration
  deriving (Show, Eq)

type ParserError = Dec
type ParserState = Text
