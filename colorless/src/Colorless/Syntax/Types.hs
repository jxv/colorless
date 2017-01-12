{-# LANGUAGE ConstraintKinds #-}
module Colorless.Syntax.Types
  ( Line(..)
  , Column(..)
  , Position(..)
  , PrimTy(..)
  , OpaqueTy(..)
  , Tag(..)
  , Fn(..)
  , Field(..)
  , Ctor(..)
  , Ty(..)
  , ParamRef(..)
  , OpaqueTyRef(..)
  , TagDecl(..)
  , FnArg(..)
  , FnDecl(..)
  , TyParam(..)
  , OpaqueDecl(..)
  , CtorDecl(..)
  , SumFieldDecl(..)
  , SumDecl(..)
  , FieldDecl(..)
  , ProductFieldDecl(..)
  , ProductDecl(..)
  , OpaqueImport(..)
  , ModuleVersion(..)
  , ModuleImport(..)
  , Import(..)
  , ImportsDecl(..)
  , Directory(..)
  , HttpDecl(..)
  , HttpMeta(..)
  , ModuleRef(..)
  , ModuleHook(..)
  , DomainRef(..)
  , DomainInstance(..)
  , DomainFieldDecl(..)
  , DomainDecl(..)
  , ServiceRef(..)
  , ServiceDecl(..)
  , ProtocolDecl(..)
  , HttpFormat(..)
  , ModuleOverrideDecl(..)
  , Decl(..)
  , ParserError
  , ParserState
  , MonadParser
  , reverseDecl
  ) where

import Pregame
import Data.String (IsString(fromString))
import Text.Megaparsec (Dec)
import Text.Megaparsec.Prim (MonadParsec)

newtype Line = Line Integer
  deriving (Show, Eq, Num)

newtype Column = Column Integer
  deriving (Show, Eq, Num)

data Position = Position
  { _line :: Line
  , _column :: Column
  } deriving (Show, Eq)

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

newtype OpaqueTy = OpaqueTy Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Tag = Tag Text
  deriving (Show, Eq, IsString, ToText)

newtype Fn = Fn Text
  deriving (Show, Eq, IsString, ToText)

newtype Field = Field Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Ctor = Ctor Text
  deriving (Show, Eq, Ord, IsString, ToText)

data Ty
  = TyPrim PrimTy
  | TyOpaque OpaqueTyRef
  deriving (Show, Eq)

data Decimal = Decimal
  { _integer :: Integer
  , _decimal :: Integer
  , _decimalResolution :: Integer
  } deriving (Show, Eq)

data ParamRef
  = ParamRefField Field
  | ParamRefString Text
  | ParamRefInteger Integer
  | ParamRefDecimal Decimal
  deriving (Show, Eq)

data OpaqueTyRef = OpaqueTyRef
  { _ty :: OpaqueTy
  , _paramRefs :: [ParamRef]
  } deriving (Show, Eq)

data TagDecl = TagDecl
  { _tag :: Tag
  , _tags :: [Tag]
  } deriving (Show, Eq)

data FnArg = FnArg
  { _field :: Maybe Field
  , _ty :: Ty
  } deriving (Show, Eq)

data FnDecl = FnDecl
  { _fn :: Fn
  , _args :: [FnArg]
  , _output :: Ty
  , _tags :: [Tag]
  } deriving (Show, Eq)

data TyParam = TyParam
  { _field :: Field
  , _primTy :: Maybe PrimTy
  } deriving (Show, Eq)

data OpaqueDecl = OpaqueDecl
  { _ty :: OpaqueTy
  , _tyParams :: [TyParam]
  } deriving (Show, Eq)

data CtorDecl = CtorDecl
  { _subtype :: Ctor
  , _params :: [Ty]
  } deriving (Show, Eq)

data SumFieldDecl
  = SumFieldDeclCtor CtorDecl
  | SumFieldDeclTag Tag
  deriving (Show, Eq)

data SumDecl = SumDecl
  { _opaqueDecl :: OpaqueDecl
  , _fields :: [SumFieldDecl]
  } deriving (Show, Eq)

data FieldDecl = FieldDecl
  { _field :: Field
  , _ty :: Ty
  } deriving (Show, Eq)

data ProductFieldDecl
  = ProductFieldDeclField FieldDecl
  | ProductFieldDeclTag Tag
  deriving (Show, Eq)

data ProductDecl = ProductDecl
  { _opaqueDecl :: OpaqueDecl
  , _fields ::  [ProductFieldDecl]
  } deriving (Show, Eq)

data OpaqueImport = OpaqueImport
  { _ty :: OpaqueTy
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
  | ImportTag Tag
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

data HttpFormat
  = HttpFormatJson
  deriving (Show, Eq)

data HttpMeta
  = HttpMetaHeader
  deriving (Show, Eq)

data HttpDecl = HttpDecl
  { _directory :: Directory
  , _format :: HttpFormat
  , _meta :: HttpMeta
  , _domain :: DomainRef
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
reverseDecl = FnDecl "reverse" [FnArg Nothing (TyPrim PrimTyStr)] (TyPrim PrimTyStr) []
