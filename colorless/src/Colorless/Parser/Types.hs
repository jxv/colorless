module Colorless.Parser.Types
  ( Line(..)
  , Column(..)
  , Position(..)
  , PrimitiveType(..)
  , OpaqueType(..)
  , Tag(..)
  , Function(..)
  , Label(..)
  , Subtype(..)
  , Type(..)
  , ParameterReference(..)
  , OpaqueTypeReference(..)
  , TagDeclaration(..)
  , FunctionParameter(..)
  , FunctionDeclaration(..)
  , TypeParameter(..)
  , OpaqueDeclaration(..)
  , SubtypeDeclaration(..)
  , SumDeclaration(..)
  , LabelDeclaration(..)
  , ProductDeclaration(..)
  , OpaqueImport(..)
  , Import(..)
  , ImportsDeclaration(..)
  , Declaration(..)
  , ParserError
  , ParserState
  ) where

import Pregame
import Text.Megaparsec (Dec)

newtype Line = Line Integer
  deriving (Show, Eq, Num)

newtype Column = Column Integer
  deriving (Show, Eq, Num)

data Position = Position
  { _line :: Line
  , _column :: Column
  } deriving (Show, Eq)

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

newtype Tag = Tag Text
  deriving (Show, Eq, IsString, ToText)

newtype Function = Function Text
  deriving (Show, Eq, IsString, ToText)

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Subtype = Subtype Text
  deriving (Show, Eq, Ord, IsString, ToText)

data Type
  = TypePrimitive PrimitiveType
  | TypeOpaque OpaqueTypeReference
  deriving (Show, Eq)

data Decimal = Decimal
  { _integer :: Integer
  , _decimal :: Integer
  , _decimalResolution :: Integer
  } deriving (Show, Eq)

data ParameterReference
  = ParameterReferenceLabel Label
  | ParameterReferenceString Text
  | ParameterReferenceInteger Integer
  | ParameterReferenceDecimal Decimal
  deriving (Show, Eq)

data OpaqueTypeReference = OpaqueTypeReference
  { _type :: OpaqueType
  , _parameterReferences :: [ParameterReference]
  } deriving (Show, Eq)

data TagDeclaration = TagDeclaration
  { _tag :: Tag
  , _tags :: [Tag]
  } deriving (Show, Eq)

data FunctionParameter = FunctionParameter
  { _label :: Maybe Label
  , _type :: Type
  } deriving (Show, Eq)

data FunctionDeclaration = FunctionDeclaration
  { _function :: Function
  , _parameters :: [FunctionParameter]
  , _output :: Type
  , _tags :: [Tag]
  } deriving (Show, Eq)

data TypeParameter = TypeParameter
  { _label :: Label
  , _primitiveType :: Maybe PrimitiveType
  } deriving (Show, Eq)

data OpaqueDeclaration = OpaqueDeclaration
  { _type :: OpaqueType
  , _typeParameters :: [TypeParameter]
  } deriving (Show, Eq)

data SubtypeDeclaration = SubtypeDeclaration
  { _subtype :: Subtype
  , _parameters :: [Type]
  } deriving (Show, Eq)

data SumDeclaration = SumDeclaration
  { _opaqueDeclaration :: OpaqueDeclaration
  , _subtypes :: [SubtypeDeclaration]
  , _tags :: [Tag]
  } deriving (Show, Eq)

data LabelDeclaration = LabelDeclaration
  { _label :: Label
  , _type :: Type
  } deriving (Show, Eq)

data ProductDeclaration = ProductDeclaration
  { _opaqueDeclaration :: OpaqueDeclaration
  , _labels ::  [LabelDeclaration]
  , _tags :: [Tag]
  } deriving (Show, Eq)

data OpaqueImport = OpaqueImport
  { _type :: OpaqueType
  , _typeParameters :: [TypeParameter]
  } deriving (Show, Eq)

data Import
  = ImportOpaque OpaqueImport
  | ImportTag Tag
  deriving (Show, Eq)

data ImportsDeclaration = ImportsDeclaration
  { _imports :: NonEmpty Import
  } deriving (Show, Eq)

data Declaration
  = DeclarationTag TagDeclaration
  | DeclarationFunction FunctionDeclaration
  | DeclarationSum SumDeclaration
  | DeclarationProduct ProductDeclaration
  | DeclarationImports ImportsDeclaration
  deriving (Show, Eq)

type ParserError = Dec

type ParserState = Text
