module Fluid.Gen.History where

import Prelude

import Data.Array as Array
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Data.StrMap as StrMap
import Fluid.Gen.Diff (SchemaDiff, schemaDiffs)
import Fluid.Gen.Spec (Spec, TypeName, Version, Schema)

type History =
  { version :: Version
  , diff :: SchemaDiff
  , spec :: Spec
  }

data Delta
  = Delta'Major
  | Delta'Minor
  | Delta'None

addSchemaDiff :: Schema -> SchemaDiff
addSchemaDiff schema = { addType: StrMap.keys schema, removeType: [], modifyType: [], sameType: [] }

createHistory :: Array Spec -> Array History
createHistory specs = case Array.head specs of
  Nothing -> []
  Just head -> let
    diffs = schemaDiffs $ Array.toUnfoldable (map (\spec -> spec.schema) specs)
    diffs'deltas = map (\diff -> Tuple diff (versionChange diff)) diffs
    initVersion = fromMaybe { major: 0, minor: 0 } head.version
    diff'delta = Tuple (addSchemaDiff head.schema) Delta'None
    asdf = Array.zipWith
      (\(Tuple diff delta) spec -> {diff, delta, spec})
      (Array.cons diff'delta diffs'deltas)
      specs
    in Array.reverse $ snd $ foldl specVersion (Tuple initVersion []) asdf

specVersion
  :: Tuple Version (Array History)
  -> { diff :: SchemaDiff, delta :: Delta, spec :: Spec }
  -> Tuple Version (Array History)
specVersion (Tuple version histories) {diff, delta, spec} = case spec.version of
  Nothing -> let
    version' = nextVersion version delta
    history = { version: version', spec, diff }
    histories' = Array.cons history histories
    in Tuple version' histories'
  Just version' -> let
    history = { version: version', spec, diff }
    histories' = Array.cons history histories
    in Tuple version' histories'

typeChanges :: SchemaDiff -> { major :: Array TypeName, minor :: Array TypeName }
typeChanges d =
  { major: Array.nub $ Array.concat [d.removeType, d.modifyType]
  , minor: d.addType
  }

nextVersion :: Version -> Delta -> Version
nextVersion {major,minor} Delta'Major = { major: major + 1, minor: 0 }
nextVersion {major,minor} Delta'Minor = { major, minor: minor + 1 }
nextVersion version Delta'None = version

versionChange :: SchemaDiff -> Delta
versionChange d
  | Array.null d.addType && Array.null d.removeType && Array.null d.modifyType = Delta'None
  | Array.null d.removeType && Array.null d.modifyType = Delta'Minor
  | otherwise = Delta'Major
