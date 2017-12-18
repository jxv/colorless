module Fluid.Gen.Haskell.Spec where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
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
  , error :: String
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
  }

type Wrap =
  { name :: String
  , label :: String
  , lowercase :: String
  , type :: String
  , func :: Maybe Func
  , instances :: { text :: Boolean, number :: Boolean }
  , major :: Int
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
  }

hollow :: Int -> Tuple TypeName HollowDecl -> Maybe Hollow
hollow _ (Tuple n {o}) = do
  let name = langTypeName n
  let label = langTypeLabel n
  let lowercase = lowercaseFirstLetter name
  output <- langType o
  let func = { name: lowercase, output }
  pure { name, label, lowercase, func }

wrap :: Int -> Tuple TypeName WrapDecl -> Maybe Wrap
wrap major (Tuple n {w: ty@(Type w), o}) = do
  let name = langTypeName n
  let label = langTypeLabel n
  let lowercase = lowercaseFirstLetter name
  type' <- langType ty
  func <- makeFunc lowercase o
  let instances = { text: isString w.n, number: isNumber w.n }
  pure { name, label, lowercase: lowercase, type: type', func, instances, major }

member :: MemberDecl -> Maybe Member
member (MemberDecl m) = do
  ty <- langType m.ty
  pure { name: langTypeName m.name, label: langTypeLabel m.name, "type": ty }

struct :: Int -> Tuple TypeName StructDecl -> Maybe Struct
struct major (Tuple n {m,o}) = do
  let name = langTypeName n
  let label = langTypeLabel n
  let lowercase = lowercaseFirstLetter name
  members <- traverse member m
  func <- makeFunc lowercase o
  pure { name, label, lowercase, members, func, major }

enumeral :: EnumDecl -> Maybe Enumeral
enumeral (EnumDecl {tag,m}) = do
  let tag' = langTypeName tag
  let label = langTypeLabel tag
  members <- case m of
    Nothing ->  Nothing
    Just m' -> do
      members' <- traverse member m'
      pure (Just members')
  pure { tag: tag', label, members }

enumeration :: Int -> Tuple TypeName EnumerationDecl -> Maybe Enumeration
enumeration major (Tuple n {e,o}) = do
  let name = langTypeName n
  let label = langTypeLabel n
  let lowercase = lowercaseFirstLetter name
  enumerals <- traverse enumeral e
  func <- makeFunc lowercase o
  pure { name, label, lowercase, enumerals, func, major }

makeFunc :: TypeName -> Maybe Type -> Maybe (Maybe { name :: TypeName, output :: String })
makeFunc name output = case map langType output of
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

langTypeOr :: Type -> PlanError -> Either PlanError String
langTypeOr a e = case langType a of
  Nothing -> Left e
  Just x -> Right x

plan
  :: String -- Prefix
  -> Version
  -> Spec
  -> Array String -- Addons
  -> (TypeName -> Int) -- Mapping type to major version
  -> String -- JSON Spec
  -> Either PlanError Plan
plan prefix version spec addons latest jsonSpec = do
  wraps <- traverse (applyLangType latest wrap) (filterWrapDecl spec.schema)
  hollows <- traverse (applyLangType latest hollow) (filterHollowDecl spec.schema)
  structs <- traverse (applyLangType latest struct) (filterStructDecl spec.schema)
  enumerations <- traverse (applyLangType latest enumeration) (filterEnumDecl spec.schema)
  meta <- langTypeOr spec.pull.error (PlanError'NonGeneratableMeta spec.pull.meta)
  error <- langTypeOr spec.pull.error (PlanError'NonGeneratableError spec.pull.error)
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
      , meta }
    , wraps
    , hollows
    , structs
    , enumerations
    , spec: jsonSpec }

primMap :: String -> Maybe String
primMap s = case s of
  "Unit" -> Just "()"
  "Bool" -> Just "P.Bool"
  "Int" -> Just "P.Int"
  "Float" -> Just "P.Double"
  "Char" -> Just "P.Char"
  "String" -> Just "R.Text"
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

langTypeLabel :: String -> String
langTypeLabel n = if n == "tag" then "_tag" else n

langTypeName :: TypeName -> String
langTypeName name = case primMap name of
  Nothing -> name
  Just name' -> name'

langTypeNameVersion :: Int -> TypeName -> String
langTypeNameVersion major name = case primMap name of
  Nothing -> "V" <> show major <> "." <> name
  Just name' -> name'

langTypeGeneric :: (TypeName -> String) -> Type -> Maybe String
langTypeGeneric typeName (Type {n, p}) = case p of
  Param'None -> Just (typeName n)
  Param'One p0 -> case langTypeGeneric typeName p0 of
    Nothing -> Nothing
    Just p0' -> case n of
      "List" -> Just $ "[" <> p0' <> "]"
      "Option" -> Just $ "(P.Maybe" <> p0' <> ")"
      _ -> Nothing
  Param'Two p0 p1 -> case Tuple (langTypeGeneric typeName p0) (langTypeGeneric typeName p1) of
    Tuple (Just p0') (Just p1') -> case n of
      "Either" -> Just $ "(P.Either (" <> p0' <> ") (" <> p1' <> "))"
      _ -> Nothing
    _ -> Nothing

langType :: Type -> Maybe String
langType = langTypeGeneric langTypeName

langTypeVersion :: Int -> Type -> Maybe String
langTypeVersion major = langTypeGeneric (langTypeNameVersion major)
