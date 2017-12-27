module Fluid.Gen.JavaScript.Client where

import Prelude
import Data.Maybe (isJust, Maybe(..))
import Data.Traversable (traverse_)

import Fluid.Gen.Lines
import Fluid.Gen.Haskell.Spec

gen :: Plan -> Array String -> String
gen p _ = linesContent do
  line "import { assert } from '../../fluid';"
  line ""
  line "export const version = {"
  addLine ["  major: ", show p.version.major, "," ]
  addLine ["  minor: ", show p.version.minor, "," ]
  line "};"

  flip traverse_ p.hollows $ \{name,label} -> do
    line ""
    addLine ["export const ", name, " = () => ({"]
    addLine ["  n: '", label, "',"]
    addLine ["});"]

  flip traverse_ p.wraps $ \{name,label,func} -> if isJust func
    then do
      line ""
      addLine ["export const ", name, " = (w) => {"]
      addLine ["  assert(w !== undefined, \"`", name, "` missing value\");"]
      line "  return {"
      addLine ["    n: '", label, "',"]
      addLine ["    w: w,"]
      addLine ["  };"]
      addLine ["};"]
    else pure unit

  flip traverse_ p.structs $ \{name,label,func,members} -> if isJust func
    then do
      line ""
      addLine ["export const ", name, " = (m) => {"]
      addLine ["  assert(m !== undefined, \"`", name, "` missing members\");"]
      flip traverse_ members $ \member ->
        addLine ["  assert(m.", member.name, " !== undefined, \"`", name, "` missing member `", member.name, "`\");"]
      line "  return {"
      addLine ["    n: '", label, "',"]
      addLine ["    m: {"]
      flip traverse_ members $ \member ->
        addLine ["      \'", member.label, "\': m.", member.name, ","]
      addLine ["    },"]
      addLine ["  };"]
      addLine ["};"]
    else pure unit

  flip traverse_ p.enumerations $ \{name,label,func,enumerals} -> if isJust func
    then do
      line ""
      addLine ["export const ", name, " = (e) => {"]
      addLine ["  assert(e !== undefined, \"`", name, "` missing enumeral\");"]
      addLine ["  assert(e.tag !== undefined, \"`", name, "` missing tag\");"]
      addLine $
        ["  assert("] <>
        map (\enumeral -> "e.tag === \'" <> enumeral.tag <> "\' || ") enumerals <>
        ["false, \"`", name, "` unrecognized tag, `\" + e.tag + \"`\");"]
      flip traverse_ enumerals $ \enumeral -> do
        addLine ["  if (e.tag === \'", enumeral.tag, "\') {"]
        case enumeral.members of
          Nothing -> pure unit
          Just members ->
            flip traverse_ members $ \member ->
              addLine ["  assert(e.", member.name, " !== undefined, \"`", name, "` with tag `", enumeral.tag, "` missing member `", member.name, "`\");"]
        addLine ["  }"]
      line "  return {"
      addLine ["    n: '", label,"',"]
      addLine ["    e: e,"]
      line "};"
    else pure unit
  line ""
