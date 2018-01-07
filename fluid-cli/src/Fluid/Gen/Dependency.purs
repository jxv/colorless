module Fluid.Gen.Dependency where

import Prelude
import Data.Set as Set
import Data.Ordering (invert)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Fluid.Gen.Diff (SchemaDiff)
import Fluid.Gen.Spec (Spec, Schema, Version, TypeName, TypeDecl(..), Type(..), MemberDecl(..), Param(..), EnumDecl(..))
import Fluid.Gen.History (History)

type DepGraph = StrMap (Set Dep)

type DepFilter = Set DepTag

buildDepGraph :: Schema -> DepGraph
buildDepGraph = map $ \tyDecl -> case tyDecl of
  TypeDecl'Hollow _ -> Set.empty
  TypeDecl'Wrap _ -> Set.empty
  TypeDecl'Struct {m} -> Set.unions $ map depsOfMemberDecl m
  TypeDecl'Enum {e} -> Set.unions $ flip map e $ \(EnumDecl enumeral) -> case enumeral.m of
    Nothing -> Set.empty
    Just m' -> Set.unions $ map depsOfMemberDecl m'
  where
    depsOfMemberDecl (MemberDecl {ty: member}) = depsByType member

depsByType :: Type -> Set Dep
depsByType (Type {n: name, p: param}) = case param of
  Param'None -> Set.singleton (Dep'Direct name)
  Param'One (Type {n: name0}) -> case name of
    "Option" -> Set.singleton (Dep'Option name0)
    "List" -> Set.singleton (Dep'List name0)
    _ -> Set.empty
  Param'Two (Type {n: name0}) (Type {n: name1}) -> case name of
    "Either" -> Set.singleton (Dep'Either name0 name1)
    _ -> Set.empty

data Dep
  = Dep'Direct TypeName
  | Dep'Option TypeName
  | Dep'List TypeName
  | Dep'Either TypeName TypeName

instance eqDep :: Eq Dep where
  eq (Dep'Direct x) (Dep'Direct x') = x == x'
  eq (Dep'Option x) (Dep'Option x') = x == x'
  eq (Dep'List x) (Dep'List x') = x == x'
  eq (Dep'Either x y) (Dep'Either x' y') = x == x' && y == y'
  eq _ _ = false

instance ordDep :: Ord Dep where
  compare (Dep'Direct x) (Dep'Direct x') = compare x x'
  compare (Dep'Direct _) (Dep'Option _) = LT
  compare (Dep'Direct _) (Dep'List _) = LT
  compare (Dep'Direct _) (Dep'Either _ _) = LT
  compare (Dep'Option x) (Dep'Option x') = compare x x'
  compare (Dep'Option _) (Dep'List _) = LT
  compare (Dep'Option _) (Dep'Either _ _) = LT
  compare (Dep'List x) (Dep'List x') = compare x x'
  compare (Dep'List _) (Dep'Either _ _) = LT
  compare (Dep'Either x y) (Dep'Either x' y') = case compare x x' of
    EQ -> compare y y'
    ordering -> ordering
  compare x y = invert (compare y x)

data DepTag
  = DepTag'Direct
  | DepTag'Option
  | DepTag'List
  | DepTag'Either

instance eqDepTag :: Eq DepTag where
  eq DepTag'Direct DepTag'Direct = true
  eq DepTag'Option DepTag'Option = true
  eq DepTag'List DepTag'List = true
  eq DepTag'Either DepTag'Either = true
  eq _ _ = false

instance ordDepTag :: Ord DepTag where
  compare DepTag'Direct DepTag'Direct = EQ
  compare DepTag'Direct DepTag'Option = LT
  compare DepTag'Direct DepTag'List = LT
  compare DepTag'Direct DepTag'Either = LT
  compare DepTag'Option DepTag'Option = EQ
  compare DepTag'Option DepTag'List = LT
  compare DepTag'Option DepTag'Either = LT
  compare DepTag'List DepTag'List = EQ
  compare DepTag'List DepTag'Either = LT
  compare DepTag'Either DepTag'Either = EQ
  compare x y = invert (compare y x)

requiresRecursiveIndirection :: DepGraph -> DepFilter -> TypeName -> Boolean
requiresRecursiveIndirection depGraph depFilter topName = go topName Set.empty
  where
    go name explored = case StrMap.lookup name depGraph of
      Nothing -> false
      Just deps -> let
        frontier = Set.difference deps explored
        explored' = explored <> deps
        frontierNames = Set.unions (Set.map noFilterDepNames frontier)
        in Set.member topName (namesOfDependencies frontier depFilter) || (Set.member true $ Set.map (flip go explored') frontierNames)

namesOfDependencies :: Set Dep -> DepFilter -> Set TypeName
namesOfDependencies deps depFilter = Set.unions $ map (\dep -> nameOfDependency dep depFilter) (Set.toUnfoldable deps :: Array Dep)

noFilterDepNames :: Dep -> Set TypeName
noFilterDepNames dep = Set.fromFoldable $ case dep of
  Dep'Direct x -> [x]
  Dep'Option x -> [x]
  Dep'List x -> [x]
  Dep'Either x y -> [x, y]

nameOfDependency :: Dep -> DepFilter -> Set TypeName
nameOfDependency dep@(Dep'Direct x) depFilter = nameOfDependencyAux dep depFilter [x]
nameOfDependency dep@(Dep'Option x) depFilter = nameOfDependencyAux dep depFilter [x]
nameOfDependency dep@(Dep'List x) depFilter = nameOfDependencyAux dep depFilter [x]
nameOfDependency dep@(Dep'Either x y) depFilter = nameOfDependencyAux dep depFilter [x, y]

nameOfDependencyAux :: Dep -> DepFilter -> Array TypeName -> Set TypeName
nameOfDependencyAux dep depFilter xs = Set.fromFoldable $ if Set.member (toDepTag dep) depFilter then xs else []

toDepTag :: Dep -> DepTag
toDepTag (Dep'Direct _) = DepTag'Direct
toDepTag (Dep'Option _) = DepTag'Option
toDepTag (Dep'List _) = DepTag'List
toDepTag (Dep'Either _ _) = DepTag'Either
