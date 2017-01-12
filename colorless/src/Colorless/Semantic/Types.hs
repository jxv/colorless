module Colorless.Semantic.Types
  ( Fn(..)
  , Arg(..)
  , Tag(..)
  , Ctor(..)
  , Field(..)
  , Opaque(..)
  , TyParam(..)
  , PolyVar(..)
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
  , HttpFormat(..)
  , HttpMeta(..)
  , HttpServiceDef(..)
  , Service(..)
  , ServiceDef(..)
  , SpecDef(..)
  , Specs(..)
  , reverseFn
  , reverseDef
  ) where

import Pregame

newtype Fn = Fn Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Arg = Arg Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Tag = Tag Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Ctor = Ctor Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Field = Field Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype Opaque = Opaque Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype TyParam = TyParam Text
  deriving (Show, Eq, Ord, IsString, ToText)

newtype PolyVar = PolyVar Text
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
  { _name :: Opaque
  , _params :: [MonoTyParamRef]
  } deriving (Show, Eq)

data FnDef = FnDef
  { _args :: [(Arg, MonoTyRef)]
  , _output :: MonoTyRef
  , _tags :: Set Tag
  } deriving (Show, Eq)

data PolyTyRef
  = PolyTyRefPrimTy PrimTy
  | PolyTyRefOpaque OpaquePolyRef
  deriving (Show, Eq)

data PolyTyParamRef
  = PolyTyParamRefPrimRef PrimRef
  | PolyTyParamRefPrimTy PrimTy
  | PolyTyParamRefOpaqueRef OpaquePolyRef
  | PolyTyParamRefPolyVar PolyVar
  deriving (Show, Eq)

data PolyTyParam = PolyTyParam
  { _name :: TyParam
  , _ref :: PolyTyParamRef
  } deriving (Show, Eq)

data OpaquePolyRef = OpaquePolyRef
  { _name :: Opaque
  , _params :: [PolyTyParamRef]
  } deriving (Show, Eq)

data AliasDef = AliasDef
  { _params :: [PolyTyParam]
  , _ref :: PolyTyRef
  , _tags :: Set Tag
  } deriving (Show, Eq)

data SumDef = SumDef
  { _params :: [PolyTyParam]
  , _ctors :: Map Ctor [PolyTyRef]
  , _tags :: Set Tag
  } deriving (Show, Eq)

data ProductDef = ProductDef
  { _params :: [PolyTyParam]
  , _fields :: Map Field PolyTyRef
  , _tags :: Set Tag
  } deriving (Show, Eq)

data OpaqueDef
  = OpaqueDefAlias AliasDef
  | OpaqueDefSum SumDef
  | OpaqueDefProduct ProductDef
  deriving (Show, Eq)

data TagDef = TagDef
  { _tags :: Set Tag
  } deriving (Show, Eq)

data Domain = Domain
  { _name :: DomainName
  , _opaques :: Map Opaque OpaqueDef
  , _fns :: Map Fn FnDef
  , _tags :: Map Tag TagDef
  } deriving (Show, Eq)

data HttpFormat
  = HttpFormatJson
  deriving (Show, Eq)

data HttpMeta
  = HttpMetaHeader
  deriving (Show, Eq)

data HttpServiceDef = HttpServiceDef
  { _dir :: HttpDir
  , _port :: HttpPort
  , _meta :: HttpMeta
  , _format :: HttpFormat
  , _domain :: Domain
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

reverseFn :: Fn
reverseFn = "reverse"

reverseDef :: FnDef
reverseDef = FnDef
  [ ("str", MonoTyRefPrimTy PrimTyStr) ]
  (MonoTyRefPrimTy PrimTyStr)
  mempty
