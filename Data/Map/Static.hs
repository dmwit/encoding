module Data.Map.Static where

import Data.Static
import Data.Array.Static

import GHC.Exts

data StaticMap i e = StaticMap (StaticArray Int i) (StaticArray Int e)

lookup :: (StaticElement i,StaticElement e,Ord i) => i -> StaticMap i e -> Maybe e
lookup ind (StaticMap idx els) = lookup' 1
    where
      lookup' n = if n > snd (bounds idx)
                  then Nothing
                  else case compare ind (idx!n) of
                         LT -> lookup' (n * 2)
                         GT -> lookup' ((n * 2) + 1)
                         EQ -> Just $ els!n

member :: (StaticElement i,StaticElement e,Ord i) => i -> StaticMap i e -> Bool
member ind (StaticMap idx _) = lookup' 1
    where
      lookup' n = if n > snd (bounds idx)
                  then False
                  else case compare ind (idx!n) of
                         LT -> lookup' (n * 2)
                         GT -> lookup' ((n * 2) + 1)
                         EQ -> True
