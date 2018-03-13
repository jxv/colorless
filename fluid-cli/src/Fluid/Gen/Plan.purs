module Fluid.Gen.Plan where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.StrMap (StrMap)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Set (Set)
import Data.Traversable (traverse)

import Fluid.Gen.Dependency (requiresRecursiveIndirection, Dep, DepTag)
import Fluid.Gen.Conversion
import Fluid.Gen.Spec (TypeName, Version, Schema, Spec, Type(..), Param(..), TypeDecl(..), HollowDecl, WrapDecl, EnumerationDecl, StructDecl, EnumDecl(..), MemberDecl(..))

type Plan =
  { prefix :: String
  , version :: Version
  , name :: String
  , lowercase :: String
  , wraps :: Array Wrap
  , hollows :: Array Hollow
  , structs :: Array Struct
  , enumerations :: Array Enumeration
  , spec :: String
  , pull :: PullPlan
  }

type PullPlan =
  { protocol :: String
  , host :: String
  , port :: Int
  , path :: String
  , meta :: String
  , metaType :: Type
  , error :: String
  , errorType :: Type
  }

data PlanError
  = PlanError'NonGeneratable TypeName
  | PlanError'NonGeneratableMeta Type
  | PlanError'NonGeneratableError Type

instance eqPlanError :: Show PlanError where
  show (PlanError'NonGeneratable name) = "Not generatable type: " <> show name
  show (PlanError'NonGeneratableMeta ty) = "Not generatable meta: " <> show ty
  show (PlanError'NonGeneratableError ty) = "Not generatable error: " <> show ty

type Func =
  { name :: String
  , output :: String
  }

type Hollow =
  { name :: String
  , label :: String
  , lowercase :: String
  , func :: Func
  , decl :: Tuple TypeName HollowDecl
  , comment :: Maybe String
  }

type Wrap =
  { name :: String
  , label :: String
  , lowercase :: String
  , type :: String
  , func :: Maybe Func
  , instances :: { text :: Boolean, number :: Boolean, float :: Boolean }
  , major :: Int
  , decl :: Tuple TypeName WrapDecl
  , comment :: Maybe String
  }

type Member =
  { name :: String
  , label :: String
  , type :: String
  }

type Struct =
  { name :: String
  , label :: String
  , lowercase :: String
  , members :: Array Member
  , func :: Maybe Func
  , major :: Int
  , indirection :: Boolean
  , decl :: Tuple TypeName StructDecl
  , comment :: Maybe String
  }

type Enumeral =
  { tag :: String
  , label :: String
  , members :: Maybe (Array Member)
  }

type Enumeration =
  { name :: String
  , lowercase :: String
  , label :: String
  , enumerals :: Array Enumeral
  , func :: Maybe Func
  , major :: Int
  , indirection :: Boolean
  , decl :: Tuple TypeName EnumerationDecl
  , comment :: Maybe String
  }

hollow :: Conversion -> Int -> Tuple TypeName HollowDecl -> Maybe Hollow
hollow conv _ decl@(Tuple n {o,c}) = do
  let name = langTypeName conv n
  let label = conv.label n
  let lowercase = lowercaseFirstLetter name
  output <- langType conv o
  let func = { name: lowercase, output }
  pure { name, label, lowercase, func, decl, comment: c }

wrap :: Conversion -> Int -> Tuple TypeName WrapDecl -> Maybe Wrap
wrap conv major decl@(Tuple n {w: ty@(Type w), o,c}) = do
  let name = langTypeName conv n
  let label = conv.label n
  let lowercase = lowercaseFirstLetter name
  type' <- langType conv ty
  func <- makeFunc conv lowercase o
  let instances = { text: isString w.n, number: isNumber w.n, float: isFloat w.n }
  pure { name: conv.ty name, label, lowercase: lowercase, type: type', func, instances, major, decl, comment: c }

member :: Conversion -> MemberDecl -> Maybe Member
member conv (MemberDecl m) = do
  ty <- langType conv m.ty
  pure { name: conv.member (langTypeName conv m.name), label: conv.label m.name, "type": ty }

struct :: Conversion -> StrMap (Set Dep) -> Set DepTag -> Int -> Tuple TypeName StructDecl -> Maybe Struct
struct conv depGraph depFilter major decl@(Tuple n {m,o,c}) = do
  let name = langTypeName conv n
  let label = conv.label n
  let lowercase = lowercaseFirstLetter name
  members <- traverse (member conv) m
  func <- makeFunc conv lowercase o
  let indirection = requiresRecursiveIndirection depGraph depFilter n
  pure { name: conv.ty name, label, lowercase, members, func, major, indirection, decl, comment: c }

enumeral :: Conversion -> EnumDecl -> Maybe Enumeral
enumeral conv (EnumDecl {tag,m}) = do
  let tag' = langTypeName conv (conv.tag tag)
  let label = conv.label tag
  members <- case m of
    Nothing -> pure Nothing
    Just m' -> do
      members' <- traverse (member conv) m'
      pure (Just members')
  pure { tag: tag', label, members }

enumeration :: Conversion -> StrMap (Set Dep) -> Set DepTag -> Int -> Tuple TypeName EnumerationDecl -> Maybe Enumeration
enumeration conv depGraph depFilter major decl@(Tuple n {e,o,c}) = do
  let name = langTypeName conv n
  let label = conv.label n
  let lowercase = lowercaseFirstLetter name
  enumerals <- traverse (enumeral conv) e
  func <- makeFunc conv lowercase o
  let indirection = requiresRecursiveIndirection depGraph depFilter n
  pure { name: conv.ty name, label, lowercase, enumerals, func, major, indirection, decl, comment: c }

makeFunc :: Conversion -> TypeName -> Maybe Type -> Maybe (Maybe { name :: TypeName, output :: String })
makeFunc conv name output = case map (langType conv) output of
  Nothing -> Just Nothing
  Just Nothing -> Nothing
  Just (Just o) -> Just (Just { name: name, output: o })

filterTypeDecl :: forall a. ((Tuple TypeName TypeDecl) -> Maybe a) -> Schema -> Array a
filterTypeDecl f schema = Array.mapMaybe f (StrMap.toUnfoldable schema)

filterHollowDecl :: Schema -> Array (Tuple TypeName HollowDecl)
filterHollowDecl = filterTypeDecl $ \(Tuple n td) -> case td of
  TypeDecl'Hollow h -> Just (Tuple n h)
  _ -> Nothing

filterWrapDecl :: Schema -> Array (Tuple TypeName WrapDecl)
filterWrapDecl = filterTypeDecl $ \(Tuple n td) -> case td of
  TypeDecl'Wrap w -> Just (Tuple n w)
  _ -> Nothing

filterEnumDecl :: Schema -> Array (Tuple TypeName EnumerationDecl)
filterEnumDecl = filterTypeDecl $ \(Tuple n td) -> case td of
  TypeDecl'Enum e -> Just (Tuple n e)
  _ -> Nothing

filterStructDecl :: Schema -> Array (Tuple TypeName StructDecl)
filterStructDecl = filterTypeDecl $ \(Tuple n td) -> case td of
  TypeDecl'Struct s -> Just (Tuple n s)
  _ -> Nothing

applyLangType :: forall a b. (TypeName -> Int) -> (Int -> Tuple String a -> Maybe b) -> Tuple String a -> Either PlanError b
applyLangType v f t@(Tuple name _) = case f (v name) t of
  Nothing -> Left (PlanError'NonGeneratable name)
  Just b -> Right b

langTypeOr :: Conversion -> Type -> PlanError -> Either PlanError String
langTypeOr conv a e = case langType conv a of
  Nothing -> Left e
  Just x -> Right x

plan
  :: Conversion
  -> StrMap (Set Dep)
  -> Set DepTag
  -> String -- Prefix
  -> Version
  -> Spec
  -> Array String -- Addons
  -> (TypeName -> Int) -- Mapping type to major version
  -> String -- JSON Spec
  -> Either PlanError Plan
plan conv depGraph depFilter prefix version spec addons latest jsonSpec = do
  wraps <- traverse (applyLangType latest (wrap conv)) (filterWrapDecl spec.schema)
  hollows <- traverse (applyLangType latest (hollow conv)) (filterHollowDecl spec.schema)
  structs <- traverse (applyLangType latest (struct conv depGraph depFilter)) (filterStructDecl spec.schema)
  enumerations <- traverse (applyLangType latest (enumeration conv depGraph depFilter)) (filterEnumDecl spec.schema)
  meta <- langTypeOr conv spec.pull.meta (PlanError'NonGeneratableMeta spec.pull.meta)
  error <- langTypeOr conv spec.pull.error (PlanError'NonGeneratableError spec.pull.error)
  pure
    { prefix
    , version
    , name: spec.pull.name
    , lowercase: lowercaseFirstLetter spec.pull.name
    , pull:
      { protocol: spec.pull.protocol
      , host: spec.pull.host
      , port: spec.pull.port
      , path: spec.pull.path
      , error
      , errorType: spec.pull.error
      , meta
      , metaType: spec.pull.meta }
    , wraps
    , hollows
    , structs
    , enumerations
    , spec: jsonSpec }

primMap :: Conversion -> String -> Maybe String
primMap {unit,bool,int,float,char,string} s = case s of
  "Unit" -> Just unit
  "Bool" -> Just bool
  "Int" -> Just int
  "Float" -> Just float
  "Char" -> Just char
  "String" -> Just string
  _ -> Nothing

lowercaseFirstLetter :: String -> String
lowercaseFirstLetter s = case Str.splitAt 1 s of
  Nothing -> s
  Just s' -> Str.toLower s'.before <> s'.after

uppercaseFirstLetter :: String -> String
uppercaseFirstLetter s = case Str.splitAt 1 s of
  Nothing -> s
  Just s' -> Str.toUpper s'.before <> s'.after

isString :: TypeName -> Boolean
isString name = name == "String"

isNumber :: TypeName -> Boolean
isNumber name = name == "Int" || name == "Float"

isFloat :: TypeName -> Boolean
isFloat name = name == "Float"

langTypeName :: Conversion -> TypeName -> String
langTypeName conv name = case primMap conv name of
  Nothing -> conv.ty name
  Just name' -> name'

langTypeNameVersion :: Conversion -> Int -> TypeName -> String
langTypeNameVersion conv@{version, ty} major name = case primMap conv name of
  Nothing -> version major (ty name)
  Just name' -> name'

langTypeGeneric :: Conversion -> (TypeName -> String) -> Type -> Maybe String
langTypeGeneric conv@{list,option,either} typeName (Type {n, p}) = case p of
  Param'None -> Just (typeName n)
  Param'One p0 -> case langTypeGeneric conv typeName p0 of
    Nothing -> Nothing
    Just p0' -> case n of
      "List" -> Just $ list p0'
      "Option" -> Just $ option p0'
      _ -> Nothing
  Param'Two p0 p1 -> case Tuple (langTypeGeneric conv typeName p0) (langTypeGeneric conv typeName p1) of
    Tuple (Just p0') (Just p1') -> case n of
      "Either" -> Just $ either p0' p1'
      _ -> Nothing
    _ -> Nothing

langType :: Conversion -> Type -> Maybe String
langType conv = langTypeGeneric conv (langTypeName conv)

langTypeVersion :: Conversion -> Int -> Type -> Maybe String
langTypeVersion conv major = langTypeGeneric conv (langTypeNameVersion conv major)
