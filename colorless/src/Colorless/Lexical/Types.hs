module Colorless.Lexical.Types
  ( Line(..)
  , Column(..)
  , LexToken(..)
  , Lex(..)
  , reverseLex
  ) where

import Pregame

newtype Line = Line Integer
  deriving (Show, Eq, Num, Enum, Real, Ord, Integral)

newtype Column = Column Integer
  deriving (Show, Eq, Num, Enum, Real, Ord, Integral)

data Loc = Loc
  { _line :: Line
  , _column :: Column
  } deriving (Show, Eq)

data LexToken = LexToken
  { _loc :: Loc
  , _lex:: Lex
  } deriving (Show, Eq)

data Lex
  = LexLowerCamelCase Text
  | LexUpperCamelCase Text
  | LexNumber Integer
  | LexNest
  | LexPeriod
  | LexMinus
  | LexNewline
  | LexOpenParen
  | LexCloseParen
  | LexColon
  | LexEar
  | LexPercent
  | LexLeftAngle
  | LexRightAngle
  | LexBang
  | LexDollar
  | LexForwardSlash
  | LexPound
  deriving (Show, Eq)

reverseLex :: [LexToken]
reverseLex =
    [ LexToken loc (LexLowerCamelCase "reverse")
    , LexToken loc (LexLowerCamelCase "str")
    , LexToken loc LexColon
    , LexToken loc (LexLowerCamelCase "str")
    ]
    where
      loc = Loc 0 0
