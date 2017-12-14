module Fluid.Gen.History where

import Prelude

import Data.Array as Array
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Fluid.Gen.Diff (SchemaDiff, schemaDiffs)
import Fluid.Gen.Spec (Spec, TypeName, Version)

type History =
  { version :: Version
  , spec :: Spec
  , stringSpec :: String
  , diff :: SchemaDiff
  }

data Delta
  = Delta'Major
  | Delta'Minor
  | Delta'None

createHistory :: Array Spec -> Array (Tuple Version Spec)
createHistory specs = case Array.head specs of
  Nothing -> []
  Just head -> let
    diffs = schemaDiffs $ Array.toUnfoldable (map (\spec -> spec.schema) specs)
    deltas = map versionChange diffs
    initVersion = fromMaybe { major: 0, minor: 0 } head.version
    in snd $ foldl
        specVersion
        (Tuple initVersion [])
        (Array.zip (Array.cons Delta'None deltas) specs)

specVersion :: Tuple Version (Array (Tuple Version Spec)) -> Tuple Delta Spec -> Tuple Version (Array (Tuple Version Spec))
specVersion (Tuple version history) (Tuple delta spec) = case spec.version of
  Nothing ->
    let version' = nextVersion version delta
    in Tuple version' (Array.cons (Tuple version' spec) history)
  Just version' -> Tuple version' (Array.cons (Tuple version' spec) history)

typeChanges :: SchemaDiff -> { major :: Array TypeName, minor :: Array TypeName }
typeChanges d =
  { major: Array.nub $ Array.concat [d.removeType, d.modifyType]
  , minor: d.addType
  }

nextVersion :: Version -> Delta -> Version
nextVersion {major,minor} Delta'Major = { major: major + 1, minor }
nextVersion {major,minor} Delta'Minor = { major, minor: minor + 1 }
nextVersion version Delta'None = version

versionChange :: SchemaDiff -> Delta
versionChange d
  | Array.null d.addType && Array.null d.removeType && Array.null d.modifyType = Delta'Minor
  | otherwise = Delta'Major
