{-# LANGUAGE MagicHash #-}
module Data.Array.Static.Builder where

import Data.Static

buildStaticArray :: (StaticElement e,Show i) => (i,i) -> [e] -> String
buildStaticArray (s,e) els = "StaticArray ("++show s++") ("++show e++") \""
                             ++concat (map (\w -> '\\':show w) (concat (map gen els)))
                             ++"\"#"

buildStaticArray' :: (StaticElement e) => [e] -> String
buildStaticArray' els = buildStaticArray (0,length els-1) els
