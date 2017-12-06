module Fluid.Gen.Diff where

import Control.Applicative
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Fluid.Gen.Spec (Schema, Type, EnumDecl, MemberDecl, TypeDecl(..))
import Prelude ((/=), ($), not, (==), (&&))

type Diff =
  { addType :: Array String
  , removeType :: Array String
  , modifyType :: Array String
  }

diff :: Schema -> Schema -> Diff
diff prev next =
  { addType: Array.difference nextKeys prevKeys
  , removeType: Array.difference prevKeys nextKeys
  , modifyType: Array.filter
      (\key ->
        case typeDeclChange <$> (StrMap.lookup key prev) <*> (StrMap.lookup key next) of
          Just x -> x
          Nothing -> false)
      nextKeys
  }
  where
    nextKeys = StrMap.keys next
    prevKeys = StrMap.keys prev

typeDeclChange :: TypeDecl -> TypeDecl -> Boolean
typeDeclChange (TypeDecl'Hollow h) (TypeDecl'Hollow h') = hollowChange h h'
typeDeclChange (TypeDecl'Wrap w) (TypeDecl'Wrap w') = wrapChange w w'
typeDeclChange (TypeDecl'Enum e) (TypeDecl'Enum e') = enumChange e e'
typeDeclChange (TypeDecl'Struct s) (TypeDecl'Struct s') = structChange s s'
typeDeclChange _ _ = true

hollowChange :: { o :: Type } -> { o :: Type } -> Boolean
hollowChange a b = a.o /= b.o

wrapChange :: { w :: Type, o :: Maybe Type } -> { w :: Type, o :: Maybe Type } -> Boolean
wrapChange {w: aW, o: Nothing} {w: bW, o: _} = aW /= bW
wrapChange {w: aW, o: aO@(Just _)} {w: bW, o: bO} = not $ aW == bW && aO == bO

enumChange :: { e :: Array EnumDecl, o :: Maybe Type } -> { e :: Array EnumDecl, o :: Maybe Type } -> Boolean
enumChange {e: aE, o: Nothing} {e: bE, o: _} = aE /= bE
enumChange {e: aE, o: aO@(Just _)} {e: bE, o: bO} = not $ aE == bE && aO == bO

structChange :: { m :: Array MemberDecl, o :: Maybe Type } -> { m :: Array MemberDecl, o :: Maybe Type } -> Boolean
structChange {m: aM, o: Nothing} {m: bM, o: _} = aM /= bM
structChange {m: aM, o: aO@(Just _)} {m: bM, o: bO} = not $ aM == bM && aO == bO
