{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser
  ( ParserM
  , runParserM
  ) where

import Pregame
import Data.Tuple (uncurry)
import Text.Megaparsec (string, ParsecT, runParserT, Token, Dec, ParseError, choice)
import Text.Megaparsec.Prim (MonadParsec)

import Colorless.Parser.Atomic
import Colorless.Parser.Types

newtype ParserM a = ParserM (ParsecT ParserError ParserState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus, MonadParsec ParserError ParserState)

runParserM :: ParserM a -> Text -> IO (Either (ParseError (Token ParserState) ParserError) a)
runParserM (ParserM m) input = runParserT m "" input

primitiveType' :: Atomic m => m PrimitiveType
primitiveType' = match $ fmap (uncurry token)
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
