module Data.CharMap where

import Data.Map.Static
import Data.Encoding.ByteSink
import Data.Encoding.Exception

import Control.Throws
import Data.Word
import Data.Char
import Prelude hiding (lookup)

data CharMap
    = Node       !Char !CharMap !CharMap
    | DeadEnd
    | LeafRange1 !Int !Word8
    | LeafRange2 !Int !Word8 !Word8 !Word8
    | LeafRange3 !Int !Word8 !Word8 !Word8 !Word8 !Word8
    | LeafRange4 !Int !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
    | LeafMap1 (StaticMap Char Word8)
    | LeafMap2 (StaticMap Char Word16)
    | LeafMap4 (StaticMap Char Word32)

mapEncode :: ByteSink m => Char -> CharMap -> m ()
mapEncode ch (Node rch l r)
              | ch < rch  = mapEncode ch l
              | otherwise = mapEncode ch r
mapEncode ch DeadEnd = throwException (HasNoRepresentation ch)
mapEncode ch (LeafRange1 bch st)
    = pushWord8 $ st + (fromIntegral ((ord ch) - bch))
mapEncode ch (LeafRange2 bch min1 min2 r2)
    = let v = (ord ch) - bch
          (w1,w2) = v `divMod` (fromIntegral r2)
      in do
        pushWord8 (fromIntegral w1 + min1)
        pushWord8 (fromIntegral w2 + min2)
mapEncode ch (LeafRange3 bch min1 min2 r2 min3 r3)
    = let v = (ord ch) - bch
          (v1,w3) = v  `divMod` (fromIntegral r3)
          (w1,w2) = v1 `divMod` (fromIntegral r2)
      in do
        pushWord8 (fromIntegral w1 + min1)
        pushWord8 (fromIntegral w2 + min2)
        pushWord8 (fromIntegral w3 + min3)
mapEncode ch (LeafRange4 bch min1 min2 r2 min3 r3 min4 r4)
    = let v = (ord ch) - bch
          (v1,w4) = v  `divMod` (fromIntegral r4)
          (v2,w3) = v1 `divMod` (fromIntegral r3)
          (w1,w2) = v2 `divMod` (fromIntegral r2)
      in do
        pushWord8 (fromIntegral w1 + min1)
        pushWord8 (fromIntegral w2 + min2)
        pushWord8 (fromIntegral w3 + min3)
        pushWord8 (fromIntegral w4 + min4)
mapEncode ch (LeafMap1 mp) = case lookup ch mp of
                               Nothing -> throwException (HasNoRepresentation ch)
                               Just v -> pushWord8 v
mapEncode ch (LeafMap2 mp) = case lookup ch mp of
                               Nothing -> throwException (HasNoRepresentation ch)
                               Just v -> pushWord16be v
mapEncode ch (LeafMap4 mp) = case lookup ch mp of
                               Nothing -> throwException (HasNoRepresentation ch)
                               Just v -> pushWord32be v


mapMember :: Char -> CharMap -> Bool
mapMember c (Node rc l r)
    | c < rc = mapMember c l
    | otherwise = mapMember c r
mapMember c DeadEnd = False
mapMember c (LeafMap1 mp) = member c mp
mapMember c (LeafMap2 mp) = member c mp
mapMember c (LeafMap4 mp) = member c mp
mapMember c _ = True
