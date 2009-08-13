{-# LANGUAGE MagicHash,FlexibleInstances #-}
module Data.Static where

import GHC.Exts
import GHC.Prim
import GHC.Word
import Data.Word
import Data.Bits
import Data.Char

class StaticElement e where
    extract :: Addr# -> Int# -> e
    gen :: e -> [Word8]

instance StaticElement Word8 where
    extract addr i = W8# (indexWord8OffAddr# addr i)
    gen w = [w]

instance StaticElement Word16 where
    extract addr i = W16# (indexWord16OffAddr# addr i)
    gen w = let r1 = fromIntegral w
                r2 = fromIntegral $ w `shiftR` 8
            in [r1,r2]

instance StaticElement Word32 where
    extract addr i = W32# (indexWord32OffAddr# addr i)
    gen w = let r1 = fromIntegral w
                r2 = fromIntegral $ w `shiftR`  8
                r3 = fromIntegral $ w `shiftR` 16
                r4 = fromIntegral $ w `shiftR` 24
            in [r1,r2,r3,r4]

instance StaticElement Char where
    extract addr i = C# (indexWideCharOffAddr# addr i)
    gen c = gen (fromIntegral (ord c)::Word32)

instance StaticElement (Maybe Char) where
    extract addr i = let v = indexWord32OffAddr# addr i
                     in if eqWord# v (int2Word# 4294967295#) -- -1 in Word32
                        then Nothing
                        else (if (I# (word2Int# v)) > 0x10FFFF
                              then error (show (I# (word2Int# v))++" is not a valid char ("++show (I# i)++")")
                              else Just (chr (I# (word2Int# v)))
                             )
    gen Nothing = gen (-1::Word32)
    gen (Just c) = gen (fromIntegral (ord c)::Word32)