module Colorless.Parser.Types
  ( PrimitiveType(..)
  , Function(..)
  , Type(..)
  , FunctionPrototype(..)
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

newtype Function = Function Text
  deriving (Show, Eq, IsString)

data Type
  = TypePrimitive PrimitiveType
  deriving (Show, Eq)

data FunctionPrototype = FunctionPrototype
  { _function :: Function
  , _parameters :: [Type]
  , _output :: Type
  } deriving (Show, Eq)

type ParserError = Dec
type ParserState = Text
