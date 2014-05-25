{-# LANGUAGE MagicHash,FlexibleInstances,BangPatterns,CPP #-}
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
    extract addr i = let !v = indexWord32OffAddr# addr i
#if __GLASGOW_HASKELL__ >= 708
                     in if isTrue# (eqWord# v (int2Word# 4294967295#)) -- -1 in Word32
#else
                     in if eqWord# v (int2Word# 4294967295#) -- -1 in Word32
#endif
                        then Nothing
                        else (if (I# (word2Int# v)) > 0x10FFFF
                              then error (show (I# (word2Int# v))++" is not a valid char ("++show (I# i)++")")
                              else Just (chr (I# (word2Int# v)))
                             )
    gen Nothing = gen (-1::Word32)
    gen (Just c) = gen (fromIntegral (ord c)::Word32)

instance StaticElement a => StaticElement (a,a) where
    extract addr i = let x1 = extract addr (i *# 2#)
                         x2 = extract addr (i *# 2# +# 1#)
                     in (x1,x2)
    gen (x1,x2) = gen x1 ++ gen x2

instance StaticElement a => StaticElement (a,a,a) where
    extract addr i = let x1 = extract addr (i *# 3#)
                         x2 = extract addr (i *# 3# +# 1#)
                         x3 = extract addr (i *# 3# +# 2#)
                     in (x1,x2,x3)
    gen (x1,x2,x3) = gen x1 ++ gen x2 ++ gen x3

instance StaticElement a => StaticElement (a,a,a,a) where
    extract addr i = let x1 = extract addr (i *# 4#)
                         x2 = extract addr (i *# 4# +# 1#)
                         x3 = extract addr (i *# 4# +# 2#)
                         x4 = extract addr (i *# 4# +# 3#)
                     in (x1,x2,x3,x4)
    gen (x1,x2,x3,x4) = gen x1 ++ gen x2 ++ gen x3 ++ gen x4
