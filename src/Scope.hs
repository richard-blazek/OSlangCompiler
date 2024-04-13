module Scope (Scope, empty, include, get, addPublic, addPrivate) where

import qualified Data.Map.Strict as Map
import Typing (Type)

data Entry = Public Type | Private Type deriving (Show, Eq, Ord)
data Scope = Scope (Map.Map String Entry) deriving (Show, Eq, Ord)

isPublic :: Entry -> Bool
isPublic (Public _) = True
isPublic (Private _) = False

nameConflict :: v -> v -> t
nameConflict _ _ = error "Name conflict"

empty :: Scope
empty = Scope Map.empty

include :: Scope -> Scope -> Scope
include (Scope other) (Scope this) = Scope union
  where added = Map.filter isPublic other
        union = Map.unionWith nameConflict added this

get :: String -> Scope -> Type
get name (Scope this) = case Map.lookup name this of
  Just (Public t) -> t
  Just (Private t) -> t
  Nothing -> error "Unknown name"

addPublic :: String -> Type -> Scope -> Scope
addPublic name t (Scope this) = Scope (Map.insertWith nameConflict name (Public t) this)

addPrivate :: String -> Type -> Scope -> Scope
addPrivate name t (Scope this) = Scope (Map.insertWith nameConflict name (Private t) this)
