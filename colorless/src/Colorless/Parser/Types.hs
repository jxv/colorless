{-# LANGUAGE ConstraintKinds #-}
module Colorless.Parser.Types
  ( Line(..)
  , Column(..)
  , Loc(..)
  --
  , Lex(..)
  , LexToken(..)
  --
  , FnName(..)
  , ArgName(..)
  , TagName(..)
  , CtorName(..)
  , FieldName(..)
  , OpaqueName(..)
  , TyParamName(..)
  , PolyVarName(..)
  , Neg(..)
  , Pos(..)
  , Nat(..)
  , Rat(..)
  , Version(..)
  , HttpDir(..)
  , HttpPort(..)
  , SpecName(..)
  , ServiceName(..)
  , DomainName(..)
  , PrimTy(..)
  , HttpFormat(..)
  , HttpMeta(..)
  --
  , PrimRaw(..)
  , MonoTyRaw(..)
  , MonoTyParamRaw(..)
  , OpaqueMonoRaw(..)
  , PolyTyRaw(..)
  , PolyTyParamRaw(..)
  , PolyTyParamPre(..)
  , OpaquePolyRaw(..)
  , TagDecl(..)
  , FnDecl(..)
  , TyParam(..)
  , OpaqueDecl(..)
  , CtorDecl(..)
  , SumFieldDecl(..)
  , FieldDecl(..)
  , ProductFieldDecl(..)
  , ProductDecl(..)
  , OpaqueImport(..)
  , ModuleVersion(..)
  , ModuleImport(..)
  , Import(..)
  , ImportsDecl(..)
  , ModuleHook(..)
  , DomainRef(..)
  , DomainInstance(..)
  , DomainFieldDecl(..)
  , DomainDecl(..)
  , Directory(..)
  , HttpDecl(..)
  , ServiceRef(..)
  , ProtocolDecl(..)
  , ServiceDecl(..)
  , ModuleOverrideDecl(..)
  , Decl(..)
  , ParserError
  , ParserState
  , MonadParser
  , reverseDecl
  --
  , PrimRef(..)
  , MonoTyRef(..)
  , MonoTyParamRef(..)
  , OpaqueMonoRef(..)
  , FnDef(..)
  , PolyTyRef(..)
  , PolyTyParamRef(..)
  , PolyTyParam(..)
  , OpaquePolyRef(..)
  , AliasDef(..)
  , SumDef(..)
  , ProductDef(..)
  , OpaqueDef(..)
  , TagDef(..)
  , Domain(..)
  , HttpServiceDef(..)
  , Service(..)
  , ServiceDef(..)
  , SpecDef(..)
  , Specs(..)
  , reverseFnName
  , reverseDef
  ) where

import Pregame
import Data.String (IsString(fromString))
import Text.Megaparsec (Dec)
import Text.Megaparsec.Prim (MonadParsec)

newtype Line = Line Integer
  deriving (Show, Eq, Num)

newtype Column = Column Integer
  deriving (Show, Eq, Num)

data Loc = Loc
  { _line :: Line
  , _column :: Column
  } deriving (Show, Eq)

--- Lexical

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

--- Syntax + Semantic

newtype FnName = FnName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype ArgName = ArgName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype TagName = TagName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype CtorName = CtorName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype FieldName = FieldName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype OpaqueName = OpaqueName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype TyParamName = TyParamName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype PolyVarName = PolyVarName Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Neg = Neg Integer
  deriving (Show, Eq, Num)

newtype Pos = Pos Integer
  deriving (Show, Eq, Num)

newtype Nat = Nat Integer
  deriving (Show, Eq, Num)

newtype Rat = Rat (Integer, Integer, Integer)
  deriving (Show, Eq)

newtype Version = Version (Integer, Integer, Integer)
  deriving (Show, Eq, Ord)

newtype HttpDir = HttpDir [Text]
  deriving (Show, Eq, Monoid)

newtype HttpPort = HttpPort Int
  deriving (Show, Eq)

newtype SpecName = SpecName Text
  deriving (Show, Eq, IsString, ToText)

newtype ServiceName = ServiceName Text
  deriving (Show, Eq, IsString, ToText)

newtype DomainName = DomainName Text
  deriving (Show, Eq, IsString, ToText)

data PrimTy
  = PrimTyUnit
  | PrimTyU8
  | PrimTyU16
  | PrimTyU32
  | PrimTyU64
  | PrimTyI8
  | PrimTyI16
  | PrimTyI32
  | PrimTyI64
  | PrimTyF32
  | PrimTyF64
  | PrimTyBool
  | PrimTyChar
  | PrimTyStr
  | PrimTyInt
  | PrimTyNeg
  | PrimTyPos
  | PrimTyNat
  | PrimTyRat
  deriving (Show, Eq)

data HttpFormat
  = HttpFormatJson
  deriving (Show, Eq)

data HttpMeta
  = HttpMetaHeader
  deriving (Show, Eq)

-- Syntax

data PrimRaw
  = PrimRawField FieldName
  | PrimRawString Text
  | PrimRawChar Char
  | PrimRawInteger Integer
  deriving (Show, Eq)

data MonoTyRaw
  = MonoTyRawPrimTy PrimTy
  | MonoTyRawOpaque OpaqueMonoRaw
  deriving (Show, Eq)

data MonoTyParamRaw
  = MonoTyParamRawPrimRef PrimRaw
  | MonoTyParamRawPrimTy PrimTy
  | MonoTyParamRawOpaque OpaqueMonoRaw
  deriving (Show, Eq)

data OpaqueMonoRaw = OpaqueMonoRaw
  { _name :: OpaqueName
  , _params :: [MonoTyParamRaw]
  } deriving (Show, Eq)

data PolyTyRaw
  = PolyTyRawPrimTy PrimTy
  | PolyTyRawOpaque OpaquePolyRaw
  deriving (Show, Eq)

data PolyTyParamRaw
  = PolyTyParamRawPrimRef PrimRaw
  | PolyTyParamRawPrimTy PrimTy
  | PolyTyParamRawOpaqueRef OpaquePolyRaw
  | PolyTyParamRawPolyVar PolyVarName
  deriving (Show, Eq)

data PolyTyParamPre = PolyTyParamPre
  { _name :: TyParamName
  , _ref :: PolyTyParamRaw
  } deriving (Show, Eq)

data OpaquePolyRaw = OpaquePolyRaw
  { _name :: OpaqueName
  , _params :: [PolyTyParamPre]
  } deriving (Show, Eq)

data TagDecl = TagDecl
  { _tag :: TagName
  , _tags :: [TagName]
  } deriving (Show, Eq)

data FnDecl = FnDecl
  { _fn :: FnName
  , _args :: [(Maybe FieldName, MonoTyRaw)]
  , _output :: MonoTyRaw
  , _tags :: [TagName]
  } deriving (Show, Eq)

data TyParam = TyParam
  { _field :: FieldName
  , _primTy :: Maybe PrimTy
  } deriving (Show, Eq)

data OpaqueDecl = OpaqueDecl
  { _name :: OpaqueName
  , _tyParams :: [TyParam]
  } deriving (Show, Eq)

data CtorDecl = CtorDecl
  { _name :: CtorName
  , _params :: [MonoTyRaw]
  } deriving (Show, Eq)

data SumFieldDecl
  = SumFieldDeclCtor CtorDecl
  | SumFieldDeclTag TagName
  deriving (Show, Eq)

data SumDecl = SumDecl
  { _opaqueDecl :: OpaqueDecl
  , _fields :: [SumFieldDecl]
  } deriving (Show, Eq)

data FieldDecl = FieldDecl
  { _field :: FieldName
  , _ty :: PolyTyRaw
  } deriving (Show, Eq)

data ProductFieldDecl
  = ProductFieldDeclField FieldDecl
  | ProductFieldDeclTag TagName
  deriving (Show, Eq)

data ProductDecl = ProductDecl
  { _opaqueDecl :: OpaqueDecl
  , _fields ::  [ProductFieldDecl]
  } deriving (Show, Eq)

data OpaqueImport = OpaqueImport
  { _name :: OpaqueName
  , _tyParams :: [TyParam]
  } deriving (Show, Eq)

newtype ModuleVersion = ModuleVersion Integer
  deriving (Show, Eq, Num)

data ModuleImport = ModuleImport
  { _module :: ModuleRef
  , _version :: ModuleVersion
  } deriving (Show, Eq)

data Import
  = ImportOpaque OpaqueImport
  | ImportTag TagName
  | ImportModuleVersion ModuleImport
  | ImportModuleNoVersion ModuleRef
  | ImportDomain DomainRef
  | ImportService ServiceRef
  deriving (Show, Eq)

data ImportsDecl = ImportsDecl
  { _imports :: NonEmpty Import
  } deriving (Show, Eq)

newtype ModuleRef = ModuleRef Text
  deriving (Show, Eq, IsString)

data ModuleHook = ModuleHook
  { _to :: ModuleRef
  , _from :: ModuleRef
  } deriving (Show, Eq)

newtype DomainRef = DomainRef Text
  deriving (Show, Eq)

data DomainInstance
  = DomainInstanceModule ModuleRef
  | DomainInstanceDomain DomainRef
  deriving (Show, Eq)

data DomainFieldDecl
  = DomainFieldDeclModuleHook ModuleHook
  | DomainFieldDeclDomainInstance DomainInstance
  deriving (Show, Eq)

data DomainDecl = DomainDecl
  { _fields :: [DomainFieldDecl]
  } deriving (Show, Eq)

newtype Directory = Directory [Text]
  deriving (Show, Eq, Monoid)

instance IsString Directory where
  fromString str = Directory [toText str]

data HttpDecl = HttpDecl
  { _directory :: Directory
  , _format :: HttpFormat
  , _meta :: HttpMeta
  , _domain :: DomainRef
  , _error :: MonoTyRaw
  } deriving (Show, Eq)

newtype ServiceRef = ServiceRef Text
  deriving (Show, Eq)

data ProtocolDecl
  = ProtocolDeclHttp HttpDecl
  deriving (Show, Eq)

data ServiceDecl = ServiceDecl
  { _service :: ServiceRef
  , _protocolDecl :: ProtocolDecl
  } deriving (Show, Eq)

data ModuleOverrideDecl = ModuleOverrideDecl
  { _module :: ModuleRef
  , _version :: ModuleVersion
  } deriving (Show, Eq)

data Decl
  = DeclTag TagDecl
  | DeclFn FnDecl
  | DeclSum SumDecl
  | DeclProduct ProductDecl
  | DeclImports ImportsDecl
  | DeclDomain DomainDecl
  | DeclService ServiceDecl
  | DeclModuleOverride ModuleOverrideDecl
  deriving (Show, Eq)

type ParserError = Dec

type ParserState = Text

type MonadParser a = MonadParsec ParserError ParserState a

reverseDecl :: FnDecl
reverseDecl = FnDecl "reverse" [(Nothing, MonoTyRawPrimTy PrimTyStr)] (MonoTyRawPrimTy PrimTyStr) []

-- Semantic

data PrimRef
  = PrimRefUnit
  | PrimRefU8 Word8
  | PrimRefU16 Word16
  | PrimRefU32 Word32
  | PrimRefU64 Word64
  | PrimRefI8 Int8
  | PrimRefI16 Int16
  | PrimRefI32 Int32
  | PrimRefI64 Int64
  | PrimRefF32 Float
  | PrimRefF64 Double
  | PrimRefBool Bool
  | PrimRefChar Char
  | PrimRefStr Text
  | PrimRefInt Integer
  | PrimRefNeg Neg
  | PrimRefPos Pos
  | PrimRefNat Nat
  | PrimRefRat Rat
  deriving (Show, Eq)

data MonoTyRef
  = MonoTyRefPrimTy PrimTy
  | MonoTyRefOpaque OpaqueMonoRef
  deriving (Show, Eq)

data MonoTyParamRef
  = MonoTyParamRefPrimRef PrimRef
  | MonoTyParamRefPrimTy PrimTy
  | MonoTyParamRefOpaqueRef OpaqueMonoRef
  deriving (Show, Eq)

data OpaqueMonoRef = OpaqueMonoRef
  { _name :: OpaqueName
  , _params :: [MonoTyParamRef]
  } deriving (Show, Eq)

data FnDef = FnDef
  { _args :: [(ArgName, MonoTyRef)]
  , _output :: MonoTyRef
  , _tags :: Set TagName
  } deriving (Show, Eq)

data PolyTyRef
  = PolyTyRefPrimTy PrimTy
  | PolyTyRefOpaque OpaquePolyRef
  deriving (Show, Eq)

data PolyTyParamRef
  = PolyTyParamRefPrimRef PrimRef
  | PolyTyParamRefPrimTy PrimTy
  | PolyTyParamRefOpaqueRef OpaquePolyRef
  | PolyTyParamRefPolyVar PolyVarName
  deriving (Show, Eq)

data PolyTyParam = PolyTyParam
  { _name :: TyParamName
  , _ref :: PolyTyParamRef
  } deriving (Show, Eq)

data OpaquePolyRef = OpaquePolyRef
  { _name :: OpaqueName
  , _params :: [PolyTyParamRef]
  } deriving (Show, Eq)

data AliasDef = AliasDef
  { _params :: [PolyTyParam]
  , _ref :: PolyTyRef
  , _tags :: Set TagName
  } deriving (Show, Eq)

data SumDef = SumDef
  { _params :: [PolyTyParam]
  , _ctors :: Map CtorName [PolyTyRef]
  , _tags :: Set TagName
  } deriving (Show, Eq)

data ProductDef = ProductDef
  { _params :: [PolyTyParam]
  , _fields :: Map FieldName PolyTyRef
  , _tags :: Set TagName
  } deriving (Show, Eq)

data OpaqueDef
  = OpaqueDefAlias AliasDef
  | OpaqueDefSum SumDef
  | OpaqueDefProduct ProductDef
  deriving (Show, Eq)

data TagDef = TagDef
  { _tags :: Set TagName
  } deriving (Show, Eq)

data Domain = Domain
  { _name :: DomainName
  , _opaques :: Map OpaqueName OpaqueDef
  , _fns :: Map FnName FnDef
  , _tags :: Map TagName TagDef
  } deriving (Show, Eq)

data HttpServiceDef = HttpServiceDef
  { _dir :: HttpDir
  , _port :: HttpPort
  , _meta :: HttpMeta
  , _format :: HttpFormat
  , _domain :: Domain
  , _error :: MonoTyRef
  } deriving (Show, Eq)

data ServiceDef
  = ServiceDefHttp HttpServiceDef
  deriving (Show, Eq)

data Service = Service
  { _name :: ServiceName
  , _serviceDef :: ServiceDef
  } deriving (Show, Eq)

data SpecDef = SpecDef
  { _name :: SpecName
  , _services :: [Service]
  } deriving (Show, Eq)

data Specs = Specs
  { _defs :: Map Version SpecDef
  } deriving (Show, Eq)

reverseFnName :: FnName
reverseFnName = "reverse"

reverseDef :: FnDef
reverseDef = FnDef
  [ ("str", MonoTyRefPrimTy PrimTyStr) ]
  (MonoTyRefPrimTy PrimTyStr)
  mempty
