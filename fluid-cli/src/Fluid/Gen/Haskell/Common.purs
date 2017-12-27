module Fluid.Gen.Haskell.Common where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse_)
import Fluid.Gen.Spec (Version)
import Fluid.Gen.Lines
import Fluid.Gen.Plan

enumeralNameTagMember :: String -> String -> String
enumeralNameTagMember name enumeral = name <> "'" <> enumeral <> "'Members"

memberName :: String -> String -> String
memberName name member = lowercaseFirstLetter name <> uppercaseFirstLetter member

filterFunc :: forall a. Array { func :: Maybe Func | a } -> Array { func :: Maybe Func | a}
filterFunc = Array.filter (\{func} -> isJust func)

genPragmas :: Lines Unit
genPragmas = lines
  [ "-- Pragmas"
  , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
  , "{-# LANGUAGE LambdaCase #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , "{-# LANGUAGE NamedFieldPuns #-}"
  , "{-# LANGUAGE TupleSections #-}"
  , "{-# LANGUAGE FlexibleContexts #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE ScopedTypeVariables #-}"
  , "{-# LANGUAGE NoImplicitPrelude #-}" ]

mkExportTypes :: Plan -> Array String
mkExportTypes plan =
  map (\x -> x.name) plan.wraps <>
  map (\x -> x.name) plan.structs <>
  Array.concat (flip map plan.enumerations $ \e ->
    [e.name] <>
    map
      (\enumeral -> enumeralNameTagMember e.name enumeral.tag)
      (Array.filter (\enumeral -> isJust enumeral.members) e.enumerals))

genVersion :: { lowercase :: String, version :: Version } -> Lines Unit
genVersion {lowercase, version} = do
  line ""
  line "-- Version"
  addLine [lowercase, "'version :: C.Version"]
  addLine [lowercase, "'version = C.Version ", show version.major, " ", show version.minor]

genPull :: { lowercase :: String, pull :: PullPlan } -> Lines Unit
genPull {lowercase, pull: {protocol, host, path, port}} = do
  line ""
  addLine [lowercase, "'pull :: C.Pull"]
  addLine [lowercase, "'pull = C.Pull \"", protocol, "\" \"", host, "\" \"", path, "\" ", show port]

genToJson :: forall a. { name :: String | a } -> Lines Unit
genToJson {name} = do
  line ""
  addLine ["instance R.ToJSON ", name, " where"]
  line "  toJSON = R.toJSON P.. C.toVal"

genFromJson :: forall a. { name :: String | a } -> Lines Unit
genFromJson {name} = do
  line ""
  addLine ["instance R.FromJSON ", name, " where"]
  lines
    [ "  parseJSON _v = do"
    , "    _x <- R.parseJSON _v"
    , "    case C.fromVal _x of"
    , "      P.Nothing -> P.mzero"
    , "      P.Just _y -> P.return _y" ]

genWrap :: Wrap -> Lines Unit
genWrap {name, type: type', label, instances: {text, number}} = do
  line ""
  addLine ["-- Wrap: ", name]
  addLine ["newtype ", name, " = ", name, " ", type']
  addLine ["  deriving (P.Eq, P.Ord, ", if text then "P.IsString, R.ToText, " else "", if number then "P.Num, " else "", " P.Show)"]

genWrapToVal :: Wrap -> Lines Unit
genWrapToVal {name} = do
  line ""
  addLine ["instance C.ToVal ", name, " where"]
  addLine ["  toVal (", name, " _w) = C.toVal _w"]

genWrapFromVal :: Wrap -> Lines Unit
genWrapFromVal {name} = do
  line ""
  addLine ["instance C.FromVal ", name, " where"]
  addLine ["  fromVal _v = ", name, " P.<$> C.fromVal _v"]

genStruct :: Struct -> Lines Unit
genStruct {name, label, members} = do
  line ""
  addLine ["-- Struct: ", name]
  addLine ["data ", name, " = ", name]
  lineList members
    "  { "
    "  , "
    (\m -> [lowercaseFirstLetter name <> uppercaseFirstLetter m.name, " :: ", m.type])
  line "  } deriving (P.Show, P.Eq)"

genStructToVal :: Struct -> Lines Unit
genStructToVal {name, members} = do
  line ""
  addLine ["instance C.ToVal ", name, " where"]
  addLine ["  toVal ", name]
  lineList members
    "    { "
    "    , "
    (\m -> [memberName name m.name])
  line "    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList"
  lineList members
    "    [ "
    "    , "
    (\m -> ["(\"", m.label, "\", C.toVal ", memberName name m.name, ")"])
  line "    ]"
  line ""

genStructFromVal :: Struct -> Lines Unit
genStructFromVal {name,members} = do
  addLine ["instance C.FromVal ", name, " where"]
  line "  fromVal = \\case"
  addLine ["    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> ", name]
  lineList members
    "      P.<$>"
    "      P.<*>"
    (\m -> [" C.getMember _m \"", m.label, "\""])
  line "    _ -> P.Nothing"

genEnumeration :: Enumeration -> Lines Unit
genEnumeration {name, enumerals} = do
  line ""
  addLine ["-- Enumeration: ", name]
  addLine ["data ", name]
  lineList enumerals "  = " "  | " (\e -> [name, "'", e.tag, if isJust e.members then " " <> name <> "'" <> e.tag <> "'Members" else ""])
  addLine ["  deriving (P.Show, P.Eq)"]
  flip traverse_ enumerals $ \enumeral ->
    case enumeral.members of
      Nothing -> pure unit
      Just members -> do
        line ""
        addLine ["data ", name, "'", enumeral.tag, "'Members = ", name, "'", enumeral.tag, "'Members"]
        lineList members
          "  { "
          "  , "
          (\m -> [lowercaseFirstLetter(name <> "'" <> enumeral.tag <> uppercaseFirstLetter m.name), " :: ", m.type])
        line "  } deriving (P.Show, P.Eq)"

genEnumerationToVal :: Enumeration -> Lines Unit
genEnumerationToVal {name,enumerals} = do
  line ""
  addLine ["instance C.ToVal ", name, " where"]
  line "  toVal = \\case"
  flip traverse_ enumerals $ \enumeral -> case enumeral.members of
    Nothing -> addLine ["    ", name, "'", enumeral.tag, " -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral \"", enumeral.label, "\" P.Nothing"]
    Just members -> do
      addLine ["    ", name, "'", enumeral.tag, " ", name, "'", enumeral.tag, "'Members"]
      lineList members
        "      { "
        "      , "
        (\m -> [memberName (name <> "'" <> enumeral.tag) m.name])
      addLine ["      } -> C.Val\'ApiVal P.$ C.ApiVal\'Enumeral P.$ C.Enumeral \"", enumeral.label, "\" P.$ P.Just P.$ R.fromList"]
      lineList members
        "      [ "
        "      , "
        (\m -> ["(\"", m.label, "\", C.toVal ", memberName (name <> "'" <> enumeral.tag) m.name, ")"])
      line "      ]"

genEnumerationFromVal :: Enumeration -> Lines Unit
genEnumerationFromVal {name,enumerals} = do
  line ""
  addLine ["instance C.FromVal ", name, " where"]
  line "  fromVal = \\case"
  line "    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of"
  flip traverse_ enumerals $ \enumeral -> case enumeral.members of
    Nothing -> addLine ["      (\"", enumeral.label, "\", P.Nothing) -> P.Just ", name, "'", enumeral.tag]
    Just members -> do
      addLine ["      (\"", enumeral.label, "\", P.Just _m') -> ", name, "'", enumeral.tag, " P.<$> (", name, "'", enumeral.tag, "'Members"]
      lineList members
        "          P.<$>"
        "          P.<*>"
        (\m -> [" C.getMember _m' \"", m.label, "\""])
      line "        )"
  line "      _ -> P.Nothing"
  line "    _ -> P.Nothing"
