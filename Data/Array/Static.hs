{-# LANGUAGE MagicHash #-}
module Data.Array.Static where

import Data.Static
import GHC.Exts
import Data.Ix

data StaticArray i e = StaticArray i i Addr#

bounds :: Ix i => StaticArray i e -> (i,i)
bounds (StaticArray s e _) = (s,e)

(!) :: (StaticElement e,Ix i) => StaticArray i e -> i -> e
(!) (StaticArray s e addr) i = let (I# ri) = index (s,e) i
                               in extract addr ri