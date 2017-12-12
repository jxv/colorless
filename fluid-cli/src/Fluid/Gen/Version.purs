module Fluid.Gen.Version
  ( applyVersionsFromSpecs
  , applyVersionsFromSpec
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Fluid.Gen.Spec (Version)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode as A
import Data.Argonaut.Encode.Class as A
import Data.Argonaut.Parser as A

applyVersionsFromSpecs :: Array (Tuple Version String) -> Either String (Tuple String (Array String))
applyVersionsFromSpecs pairs = do
  specs <- traverse applyVersion pairs
  pure $ Tuple (A.stringify $ A.encodeJson specs) (map A.stringify specs)

applyVersionsFromSpec :: Array Version -> String -> Either String (Tuple String (Array String))
applyVersionsFromSpec versions spec = do
  spec' <- A.jsonParser spec
  case A.toArray spec' of
    Nothing -> Left "Not an array of specs"
    Just specs -> do
      jsons <- traverse (\(Tuple v s) -> applyVersionToJson v s) (Array.zip versions specs)
      pure $ Tuple (A.stringify $ A.fromArray jsons) (map A.stringify jsons)

applyVersion :: Tuple Version String -> Either String Json
applyVersion (Tuple version spec) = do
  spec' <- A.jsonParser spec
  applyVersionToJson version spec'

applyVersionToJson :: Version -> Json -> Either String Json
applyVersionToJson version spec = do
  obj <- A.decodeJson spec
  let obj' = StrMap.insert "version" (encodeVersion version) obj
  pure $ A.encodeJson obj'
  where
    encodeVersion {major,minor} = A.encodeJson $ StrMap.fromFoldable
      [ Tuple "major" major
      , Tuple "minor" minor ]
