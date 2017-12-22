module Fluid.Gen.Haskell.Common where

import Prelude
import Data.Array as Array
import Data.Maybe (isJust)
import Fluid.Gen.Lines
import Fluid.Gen.Haskell.Spec

enumeralNameTagMember :: String -> String -> String
enumeralNameTagMember name enumeral = name <> "\'" <> enumeral <> "'Members"

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
