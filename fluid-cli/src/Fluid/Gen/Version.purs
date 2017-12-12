module Fluid.Gen.Version where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Path.Pathy (FileName(..), extension)
import Data.StrMap as StrMap
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Fluid.Gen.Diff (Diff, diffs)
import Fluid.Gen.Spec (parseSpecs, Spec, TypeName, Version, Schema, Bridge)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode as A
import Data.Argonaut.Encode as A
import Data.Argonaut.Parser as A

applyVersionsOnSpecs :: Array (Tuple Version String) -> Either String (Tuple String (Array String))
applyVersionsOnSpecs pairs = do
  specs <- traverse applyVersion pairs
  pure $ Tuple (A.stringify $ A.encodeJson specs) (map A.stringify specs)

applyVersion :: Tuple Version String -> Either String Json
applyVersion (Tuple version spec) = do
  spec' <- A.jsonParser spec
  obj <- A.decodeJson spec'
  let obj' = StrMap.insert "version" (encodeVersion version) obj
  pure $ A.encodeJson obj'
  where
    encodeVersion {major,minor} = A.encodeJson $ StrMap.fromFoldable
      [ Tuple "major" major
      , Tuple "minor" minor ]
