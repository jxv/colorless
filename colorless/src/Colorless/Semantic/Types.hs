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
  , PrimDeref(..)
  , MonoTyDeref(..)
  , MonoTyParamDeref(..)
  , OpaqueMonoDeref(..)
  , FnDef(..)
  , PolyTyDeref(..)
  , PolyTyParamDeref(..)
  , PolyTyParam(..)
  , OpaquePolyDeref(..)
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

data PrimDeref
  = PrimDerefUnit
  | PrimDerefU8 Word8
  | PrimDerefU16 Word16
  | PrimDerefU32 Word32
  | PrimDerefU64 Word64
  | PrimDerefI8 Int8
  | PrimDerefI16 Int16
  | PrimDerefI32 Int32
  | PrimDerefI64 Int64
  | PrimDerefF32 Float
  | PrimDerefF64 Double
  | PrimDerefBool Bool
  | PrimDerefChar Char
  | PrimDerefStr Text
  | PrimDerefInt Integer
  | PrimDerefNeg Neg
  | PrimDerefPos Pos
  | PrimDerefNat Nat
  | PrimDerefRat Rat
  deriving (Show, Eq)

data MonoTyDeref
  = MonoTyDerefPrimTy PrimTy
  | MonoTyDerefOpaque OpaqueMonoDeref
  deriving (Show, Eq)

data MonoTyParamDeref
  = MonoTyParamDerefPrimDeref PrimDeref
  | MonoTyParamDerefPrimTy PrimTy
  | MonoTyParamDerefOpaqueDeref OpaqueMonoDeref
  deriving (Show, Eq)

data OpaqueMonoDeref = OpaqueMonoDeref
  { _name :: Opaque
  , _params :: [MonoTyParamDeref]
  } deriving (Show, Eq)

data FnDef = FnDef
  { _args :: [(Arg, MonoTyDeref)]
  , _output :: MonoTyDeref
  , _tags :: Set Tag
  } deriving (Show, Eq)

data PolyTyDeref
  = PolyTyDerefPrimTy PrimTy
  | PolyTyDerefOpaque OpaquePolyDeref
  deriving (Show, Eq)

data PolyTyParamDeref
  = PolyTyParamDerefPrimDeref PrimDeref
  | PolyTyParamDerefPrimTy PrimTy
  | PolyTyParamDerefOpaqueDeref OpaquePolyDeref
  | PolyTyParamDerefPolyVar PolyVar
  deriving (Show, Eq)

data PolyTyParam = PolyTyParam
  { _name :: TyParam
  , _ref :: PolyTyParamDeref
  } deriving (Show, Eq)

data OpaquePolyDeref = OpaquePolyDeref
  { _name :: Opaque
  , _params :: [PolyTyParamDeref]
  } deriving (Show, Eq)

data AliasDef = AliasDef
  { _params :: [PolyTyParam]
  , _ref :: PolyTyDeref
  , _tags :: Set Tag
  } deriving (Show, Eq)

data SumDef = SumDef
  { _params :: [PolyTyParam]
  , _ctors :: Map Ctor [PolyTyDeref]
  , _tags :: Set Tag
  } deriving (Show, Eq)

data ProductDef = ProductDef
  { _params :: [PolyTyParam]
  , _fields :: Map Field PolyTyDeref
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
  , _error :: MonoTyDeref
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
  [ ("str", MonoTyDerefPrimTy PrimTyStr) ]
  (MonoTyDerefPrimTy PrimTyStr)
  mempty
