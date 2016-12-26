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
import Text.Megaparsec (Dec)

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
  | TypeOpaque OpaqueTypeReference
  deriving (Show, Eq)

data Parameter
  = ParameterLabel Label
  | ParameterString Text
  | ParameterInteger Integer
  | ParameterRational Integer Integer
  deriving (Show, Eq)

data OpaqueTypeReference = OpaqueTypeReference
  { _type :: OpaqueType
  , _valueParameters :: [Parameter]
  } deriving (Show, Eq)

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
  , _tags :: [Tag]
  } deriving (Show, Eq)

data FunctionDeclaration = FunctionDeclaration
  { _function :: Function
  , _parameters :: [(Maybe Label, Type)]
  , _output :: Type
  , _tags :: [Tag]
  } deriving (Show, Eq)

data OpaqueDeclaration = OpaqueDeclaration
  { _type :: OpaqueType
  , _parameters :: [(Label, Maybe PrimitiveType)]
  } deriving (Show, Eq)

data SumDeclaration = SumDeclaration
  { _opaqueDeclaration :: OpaqueDeclaration
  , _subtypes :: [(SumTag, [Type])]
  , _tags :: [Tag]
  } deriving (Show, Eq)

data ProductDeclaration = ProductDeclaration
  { _opaqueDeclaration :: OpaqueDeclaration
  , _labels ::  [(Label, Type)]
  , _tags :: [Tag]
  } deriving (Show, Eq)

data ImportDeclaration = ImportDeclaration
  { _types :: NonEmpty OpaqueDeclaration
  } deriving (Show, Eq)

data Declaration
  = DeclarationTag TagDeclaration
  | DeclarationFunction FunctionDeclaration
  | DeclarationSum SumDeclaration
  | DeclarationProduct ProductDeclaration
  deriving (Show, Eq)

type ParserError = Dec
type ParserState = Text
