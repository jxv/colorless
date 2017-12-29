module Fluid.Gen.PureScript.Client where

import Prelude
import Data.Array as Array
import Data.Maybe
import Data.Foldable (sequence_)
import Data.Traversable (traverse_)
import Fluid.Gen.Lines
import Fluid.Gen.PureScript.Common
import Fluid.Gen.Plan
import Fluid.Gen.Spec (Version)

mkExportValues :: Plan -> Array String
mkExportValues p =
  map (\{name} -> p.lowercase <> "'" <> name) p.hollows <>
  map (\{name} -> p.lowercase <> "'" <> name) (filterFunc p.wraps) <>
  map (\{name} -> p.lowercase <> "'" <> name) (filterFunc p.structs) <>
  map (\{name} -> p.lowercase <> "'" <> name) (filterFunc p.enumerations) <>

  map (\{lowercase} -> lowercase <> "'Mk") p.structs <>
  map (\{lowercase} -> lowercase <> "'Mk") p.wraps <>
  Array.concatMap
    (\{lowercase, enumerals} -> map (\{tag} -> lowercase <> "'" <> tag <> "'Mk") enumerals)
    p.enumerations <>

  map (\{lowercase} -> lowercase <> "'") p.structs <>
  map (\{lowercase} -> lowercase <> "'") p.wraps <>
  map (\{lowercase} -> lowercase <> "'") p.enumerations <>

  Array.concatMap
    (\{lowercase, members} -> map (\{name} -> lowercase <> "'" <> name) members)
    p.structs <>

  Array.concatMap
    (\{lowercase, enumerals} -> flip Array.concatMap enumerals $ \enumeral -> case enumeral.members of
      Nothing -> []
      Just members -> map (\{name} -> lowercase <> "'" <> enumeral.tag <> "'" <> name) members)
    p.enumerations <>

  map (\{lowercase} -> lowercase <> "'Match") p.enumerations

genModule :: { name :: String, lowercase :: String, prefix :: String, version :: Version, types :: Array String, values :: Array String } -> Lines Unit
genModule {name, lowercase, prefix, version, types, values} = do
  line ""
  line "-- Module"
  addLine ["module ", prefix]
  addLine ["  ( ", lowercase, "'version"]
  addLine ["  , ", lowercase, "'pull"]
  addLine ["  , ", lowercase, "'request"]
  flip traverse_ types $ \ty ->
    addLine ["  , ", ty, "(..)"]
  flip traverse_ values $ \val ->
    addLine ["  , ", val]
  line "  ) where"
  line ""

genRequest :: Plan -> Lines Unit
genRequest p = do
  line ""
  addLine [p.lowercase, "'request :: (Ast.ToAst a, C.HasType a, R.FromJSON a) => ", p.pull.meta ," -> C.Expr a -> C.Request ", p.pull.meta, " a"]
  addLine [p.lowercase, "'request _meta _query = C.Request (C.Version ", show p.version.major, " ", show p.version.minor, ") ", p.lowercase, "'version _meta _query"]

genService :: Plan -> Lines Unit
genService p = do
  flip traverse_ p.hollows $ \call -> do
    line ""
    addLine [p.lowercase, "'", call.name, " :: C.Expr ", call.func.output]
    addLine [p.lowercase, "'", call.name, " = C.unsafeExpr (Ast.Ast'HollowCall (Ast.HollowCall \"", call.label, "\"))"]
  flip traverse_ p.wraps $ \call -> case call.func of
    Nothing -> pure unit
    Just func -> do
      line ""
      addLine [p.lowercase, "'", call.name, " :: C.Expr ", call.name, " -> C.Expr ", func.output]
      addLine [p.lowercase, "'", call.name, " = C.unsafeExpr P.. Ast.Ast'WrapCall P.. Ast.WrapCall \"", call.label, "\" P.. Ast.toAst"]
  flip traverse_ p.structs $ \call -> case call.func of
    Nothing -> pure unit
    Just func -> do
      line ""
      addLine [p.lowercase, "'", call.name, " :: C.Expr ", call.name, " -> C.Expr ", func.output]
      addLine [p.lowercase, "'", call.name, " = C.unsafeExpr P.. Ast.Ast'StructCall P.. Ast.StructCall \"", call.label, "\" P.. Ast.toAst"]
  flip traverse_ p.enumerations $ \call -> case call.func of
    Nothing -> pure unit
    Just func -> do
      line ""
      addLine [p.lowercase, "'", call.name, " :: C.Expr ", call.name, " -> C.Expr ", func.output]
      addLine [p.lowercase, "'", call.name, " = C.unsafeExpr P.. Ast.Ast'EnumerationCall P.. Ast.EnumerationCall \"", call.label, "\" P.. Ast.toAst"]

genWrapExpr :: Wrap -> Lines Unit
genWrapExpr w = do
  line ""
  addLine [w.lowercase, "'Mk :: C.Expr (", w.type, " -> ", w.name, ")"]
  addLine [w.lowercase, "'Mk = C.unsafeWrapExpr"]
  line ""
  addLine [w.lowercase, "' :: ", w.name, " -> C.Expr ", w.name, ""]
  addLine [w.lowercase, "' = C.unsafeExpr P.. Ast.toAst"]

genWrapToAst :: Wrap -> Lines Unit
genWrapToAst {name} = do
  line ""
  addLine ["instance Ast.ToAst ", name, " where"]
  addLine ["  toAst (", name, " _w) = Ast.toAst _w"]

genStructPath :: Struct -> Lines Unit
genStructPath {name,lowercase,members} = flip traverse_ members $ \member -> do
  line ""
  addLine [lowercase, "'", member.name, " :: C.Path (", name, " -> ", member.type, ")"]
  addLine [lowercase, "'", member.name, " = C.unsafePath [\"", member.label, "\"]"]

genStructToAst :: Struct -> Lines Unit
genStructToAst {name,lowercase,label,members} = do
  line ""
  addLine ["instance Ast.ToAst ", name, " where"]
  addLine ["  toAst ", name]
  lineList members
    "    { "
    "    , "
    (\member -> [lowercaseFirstLetter name, uppercaseFirstLetter member.name])
  line "    } = Ast.Ast'Struct P.. Ast.Struct P.$ R.fromList"
  lineList members
    "    [ "
    "    , "
    (\member -> ["(\"", member.label, "\", Ast.toAst ", lowercaseFirstLetter name, uppercaseFirstLetter member.name, ")"])
  line "    ]"

genStructExpr :: Struct -> Lines Unit
genStructExpr {name,lowercase,members} = do
  case Array.uncons members of
    Nothing -> pure unit
    Just {head,tail} -> do
      line ""
      addLine $
        [lowercase, "'Mk :: C.Expr (", head.type] <>
        map (\member -> " -> " <> member.type) tail <>
        [" -> ", name, ")"]
      addLine $
        [lowercase, "'Mk = C.unsafeStructExpr [\"", head.label, "\""] <>
        map (\member -> ", \"" <> member.label <> "\"") tail <>
        ["]"]
  line ""
  addLine [lowercase, "' :: ", name, " -> C.Expr ", name]
  addLine [lowercase, "' = C.unsafeExpr P.. Ast.toAst"]

genEnumerationPath :: Enumeration -> Lines Unit
genEnumerationPath {name, lowercase, enumerals} = flip traverse_ enumerals $ \{tag, members} -> case members of
  Nothing -> pure unit
  Just members' -> flip traverse_ members' $ \member -> do
    line ""
    addLine [lowercase, "'", tag, "'", member.name, " :: C.Path (", name, "'", tag, "'Members -> ", member.type, ")"]
    addLine [lowercase, "'", tag, "'", member.name, " = C.unsafePath [\"", member.label, "\"]"]

genEnumerationMatch :: Enumeration -> Lines Unit
genEnumerationMatch e = do
  line ""
  addLine [e.lowercase, "'Match"]
  line "  :: (C.HasType a, Ast.ToAst a)"
  addLine ["  => C.Expr ", e.name]
  flip traverse_ e.enumerals $ \enumeral -> do
    case enumeral.members of
      Nothing -> addLine ["  -> C.Expr a -- ", enumeral.tag]
      Just _ -> addLine ["  -> (C.Symbol, C.Expr ", e.name, "'", enumeral.tag, "'Members -> C.Expr a) -- | ", enumeral.tag]
  line "  -> C.Expr a"
  addLine $
    [e.lowercase, "'Match _enumeral"] <>
    map (\{tag} -> " _" <> tag) e.enumerals <>
    [" = C.unsafeExpr P.$ Ast.Ast'Match P.$ Ast.Match (Ast.toAst _enumeral)"]
  lineList e.enumerals
    "  [ "
    "  , "
    (\{members, label, tag} -> case members of
        Nothing -> ["Ast.MatchCase'Tag \"", label, "\" (Ast.toAst _", tag, ")"]
        Just members' -> let
          sym = "(P.fst _" <> tag <> ")"
          ref = "(C.unsafeExpr P.$ Ast.Ast'Ref P.$ Ast.Ref " <> sym <> ")"
          in ["Ast.MatchCase'Members \"", label, "\" ", sym, " (Ast.toAst P.$ P.snd _", tag, " ", ref, ")"])
  line "  ]"
  line ""

genEnumerationToAst :: Enumeration -> Lines Unit
genEnumerationToAst e = do
  line ""
  addLine ["instance Ast.ToAst ", e.name, " where"]
  line "  toAst = \\case"
  flip traverse_ e.enumerals $ \enumeral -> case enumeral.members of
    Nothing -> do
      addLine ["    ", e.name, "'", enumeral.tag, " -> Ast.Ast'Enumeral P.$ Ast.Enumeral \"", enumeral.label, "\" P.Nothing"]
    Just members -> do
      addLine ["    ", e.name, "'", enumeral.tag, " ", e.name, "'", enumeral.tag, "'Members"]
      lineList members
        "      { "
        "      , "
        (\member -> [e.lowercase, "'", enumeral.tag, uppercaseFirstLetter member.name])
      addLine ["      } -> Ast.Ast'Enumeral P.$ Ast.Enumeral \"", enumeral.label, "\" P.$ P.Just P.$ R.fromList" ]
      lineList members
        "      [ "
        "      , "
        (\member -> ["(\"", member.label, "\", Ast.toAst ", e.lowercase, "'", enumeral.tag, uppercaseFirstLetter member.name, ")"])
      line "      ]"


genEnumeralExpr :: Enumeration -> Lines Unit
genEnumeralExpr {name,lowercase,enumerals} = do
  line ""
  flip traverse_ enumerals $ \enumeral -> case bind enumeral.members Array.uncons of
    Nothing -> do
      addLine [lowercase, "'", enumeral.tag, "'Mk :: C.Expr ", name]
      addLine [lowercase, "'", enumeral.tag, "'Mk = C.unsafeExpr P.. Ast.toAst P.$ ", name, "'", enumeral.tag]
      line ""
    Just {head,tail} -> do
      let members = [head] <> tail
      addLine $
        [lowercase, "'", enumeral.tag, "'Mk :: C.Expr (", head.type] <>
        map (\member -> " -> " <> member.type) tail <>
        [" -> ", name, ")"]
      addLine $
        [lowercase, "'", enumeral.tag, "'Mk = C.unsafeEnumeralExpr \"", enumeral.label, "\" [\"", head.label, "\""] <>
        map (\member -> ", \"" <> member.label <> "\"") tail <>
        ["]"]
      line ""
  addLine [lowercase, "' :: ", name, " -> C.Expr ", name]
  addLine [lowercase, "' = C.unsafeExpr P.. Ast.toAst"]

genToExpr :: forall a. { name :: String | a } -> Lines Unit
genToExpr {name} = do
  line ""
  addLine ["instance C.ToExpr ", name]

genImports :: { importing :: Array (Lines Unit) } -> Lines Unit
genImports {importing} = do
    line "-- Imports"
    line "import qualified Prelude as P"
    line "import qualified Control.Monad as P"
    line "import qualified Data.String as P (IsString)"
    line "import qualified Data.IORef as IO"
    line "import qualified Fluid.Client as C"
    line "import qualified Fluid.Client.Expr as C"
    line "import qualified Fluid.Ast as Ast"
    line "import qualified Fluid.Imports as R"
    sequence_ importing

type Addon =
  { exporting :: Array String
  , importing :: Lines Unit
  , gen :: Lines Unit }

httpClientAddon :: Plan -> Addon
httpClientAddon p =
  { importing: line "import qualified Fluid.Client.HttpClient as HttpClient"
  , exporting: [p.lowercase <> "'HttpClient'Post"]
  , gen: do
      line ""
      addLine [p.lowercase, "'HttpClient'Post"]
      line "  :: (C.HasType a, Ast.ToAst a, C.FromVal a)"
      line "  => HttpClient.Manager"
      line "  -> C.Pull"
      line "  -> HttpClient.RequestHeaders"
      addLine ["  -> C.Request ", p.pull.meta," a"]
      addLine ["  -> P.IO (HttpClient.HttpClientResponse R.ByteString, P.Maybe (C.Response ", p.pull.error, " a))"]
      addLine [p.lowercase, "'HttpClient'Post = HttpClient.sendRequest"]
  }

createAddon :: Plan -> String -> Maybe Addon
createAddon plan addon = case addon of
  "http-client" -> Just (httpClientAddon plan)
  _ -> Nothing

genHasType :: forall a. { name :: String, label :: String | a } -> Lines Unit
genHasType {name,label} = do
  line ""
  addLine ["instance C.HasType ", name, " where"]
  addLine ["  getType _ = \"", label, "\""]

gen :: Plan -> Array String -> String
gen plan addonNames = linesContent do
  let exportTypes = mkExportTypes plan
  let exportValues = mkExportValues plan
  let addons = Array.catMaybes $ map (createAddon plan) addonNames
  let addonExporting = Array.concatMap (\x -> x.exporting) addons
  let addonImporting = map (\x -> x.importing) addons

  genModule
    { name: plan.name
    , lowercase: plan.lowercase
    , prefix: plan.prefix
    , version: plan.version
    , types: exportTypes
    , values: addonExporting <> exportValues }
  genImports { importing: addonImporting }

  line ""
  line "--------------------------------------------------------"
  line "-- Configs"
  line "--------------------------------------------------------"

  genVersion
    { lowercase: plan.lowercase
    , version: plan.version }
  genPull
    { lowercase: plan.lowercase
    , pull: plan.pull }

  line ""
  line "--------------------------------------------------------"
  line "-- Types"
  line "--------------------------------------------------------"

  traverse_ genWrap plan.wraps
  traverse_ genStruct plan.structs
  traverse_ genEnumeration plan.enumerations

  line ""
  line "--------------------------------------------------------"
  line "-- API"
  line "--------------------------------------------------------"

  genRequest plan
  genService plan

  flip traverse_ plan.wraps $ \ty -> do
    genWrapExpr ty

  flip traverse_ plan.structs $ \ty -> do
    genStructExpr ty
    genStructPath ty

  flip traverse_ plan.enumerations $ \ty -> do
    genEnumeralExpr ty
    genEnumerationPath ty

  line ""
  line "--------------------------------------------------------"
  line "-- Add-ons"
  line "--------------------------------------------------------"

  traverse_ (\addon -> addon.gen) addons

  line ""
  line "--------------------------------------------------------"
  line "-- Type Instances"
  line "--------------------------------------------------------"

  flip traverse_ plan.wraps $ \ty -> do
    genHasType ty
    genWrapToVal ty
    genWrapFromVal ty
    genToExpr ty
    genToJson ty
    genFromJson ty
    genWrapToAst ty

  flip traverse_ plan.structs $ \ty -> do
    genHasType ty
    genStructToVal ty
    genStructFromVal ty
    genToExpr ty
    genToJson ty
    genFromJson ty
    genStructToAst ty

  flip traverse_ plan.enumerations $ \ty -> do
    genHasType ty
    flip traverse_ ty.enumerals $ \enumeral -> case enumeral.members of
      Nothing -> pure unit
      Just members -> genHasType { name: ty.name <> "'" <> enumeral.tag <> "'Members", label: ty.label }
    genEnumerationToVal ty
    genEnumerationFromVal ty
    genToExpr ty
    genToJson ty
    genFromJson ty
    genEnumerationToAst ty
    genEnumerationMatch ty

  line ""
