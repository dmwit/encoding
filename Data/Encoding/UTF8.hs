{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements UTF-8 encoding and decoding as in RFC 3629.
     See <http://en.wikipedia.org/wiki/UTF-8> for more information.
 -}
module Data.Encoding.UTF8 where

import Control.Throws
import Data.Char
import Data.Bits

import Data.Encoding.Base
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink
import Data.Encoding.Exception

import Data.Typeable

data UTF8 = UTF8        -- ^ Very forgiving decoding mechanism, accepts everything that it can make any sense of.
          | UTF8Strict  -- ^ More strict decoding, doesn\'t accept sequences that have a too long representation and checks bits that aren\'t used in the decoding
          deriving (Eq,Show,Typeable)

instance Encoding UTF8 where
    encodeChar _ c
               | n <= 0x0000007F = p8 n
               | n <= 0x000007FF = do
                         p8 $ 0xC0 .|. (n `shiftR` 6)
                         p8 $ 0x80 .|. (n .&. 0x3F)
               | n <= 0x0000FFFF = do
                         p8 $ 0xE0 .|. (n `shiftR` 12)
                         p8 $ 0x80 .|. ((n `shiftR` 6) .&. 0x3F)
                         p8 $ 0x80 .|. (n .&. 0x3F)
               | n <= 0x0010FFFF = do
                         p8 $ 0xF0 .|. (n `shiftR` 18)
                         p8 $ 0x80 .|. ((n `shiftR` 12) .&. 0x3F)
                         p8 $ 0x80 .|. ((n `shiftR` 6) .&. 0x3F)
                         p8 $ 0x80 .|. (n .&. 0x3F)
               | otherwise = throwException (HasNoRepresentation c)
               where
                 n = ord c
                 p8 = pushWord8.fromIntegral
    encodeable _ c = c <= '\x10FFFF'
    decodeChar UTF8 = do
      w1 <- fetchWord8
      case () of 
        _
          | w1 <= 0x7F -> return $ chr $ fromIntegral w1
          | w1 <= 0xBF -> throwException (IllegalCharacter w1)
          | w1 <= 0xDF -> do
                         w2 <- fetchWord8
                         return $ chr $
                                    ((fromIntegral $ w1 .&. 0x1F) `shiftL` 6)
                                    .|. (fromIntegral $ w2 .&. 0x3F)

          | w1 <= 0xEF -> do
                         w2 <- fetchWord8
                         w3 <- fetchWord8
                         let v1 = w1 .&. 0x0F
                             v2 = w2 .&. 0x3F
                             v3 = w3 .&. 0x3F
                         return $ chr $
                                    ((fromIntegral v1) `shiftL` 12)
                                    .|. ((fromIntegral v2) `shiftL` 6)
                                    .|. (fromIntegral v3)
          | w1 <= 0xF7 -> do
                         w2 <- fetchWord8
                         w3 <- fetchWord8
                         w4 <- fetchWord8
                         let v1 = w1 .&. 0x07
                             v2 = w2 .&. 0x3F
                             v3 = w3 .&. 0x3F
                             v4 = w4 .&. 0x3F
                             v  = ((fromIntegral v1) `shiftL` 18)
                                    .|. ((fromIntegral v2) `shiftL` 12)
                                    .|. ((fromIntegral v3) `shiftL` 6)
                                    .|. (fromIntegral v4)
                         if v <= 0x10FFFF
                           then return $ chr v
                           else throwException (IllegalRepresentation [w1,w2,w3,w4])
          | otherwise -> throwException (IllegalCharacter w1)
    decodeChar UTF8Strict = do
      w1 <- fetchWord8
      case () of 
        _
          | w1 <= 0x7F -> return $ chr $ fromIntegral w1
          | w1 <= 0xBF -> throwException (IllegalCharacter w1)
          | w1 <= 0xDF -> do
                         w2 <- fetchExtend8
                         let v1 = w1 .&. 0x1F
                         if v1 <= 1
                           then throwException (IllegalRepresentation [w1,w2])
                           else return $ chr $ 
                                    ((fromIntegral v1) `shiftL` 6)
                                    .|. (fromIntegral $ w2 .&. 0x3F)
          | w1 <= 0xEF -> do
                         w2 <- fetchExtend8
                         w3 <- fetchExtend8
                         let v1 = w1 .&. 0x0F
                             v2 = w2 .&. 0x3F
                             v3 = w3 .&. 0x3F
                         if v1 == 0 && v2 < 0x20
                           then throwException (IllegalRepresentation [w1,w2,w3])
                           else return $ chr $
                                    ((fromIntegral v1) `shiftL` 12)
                                    .|. ((fromIntegral v2) `shiftL` 6)
                                    .|. (fromIntegral v3)
          | w1 <= 0xF7 -> do
                         w2 <- fetchExtend8
                         w3 <- fetchExtend8
                         w4 <- fetchExtend8
                         let v1 = w1 .&. 0x07
                             v2 = w2 .&. 0x3F
                             v3 = w3 .&. 0x3F
                             v4 = w4 .&. 0x3F
                             v = ((fromIntegral v1) `shiftL` 18)
                                    .|. ((fromIntegral v2) `shiftL` 12)
                                    .|. ((fromIntegral v3) `shiftL` 6)
                                    .|. (fromIntegral v4)
                         if v1 == 0 && v2 < 0x10
                           then throwException (IllegalRepresentation [w1,w2,w3,w4])
                           else (if v <= 0x10FFFF
                                 then return $ chr v
                                 else throwException (IllegalRepresentation [w1,w2,w3,w4]))
          | otherwise -> throwException (IllegalCharacter w1)
          where
            invalidExtend wrd = wrd .&. 0xC0 /= 0x80
            fetchExtend8 = do
              w <- fetchWord8
              if invalidExtend w 
                then throwException (IllegalCharacter w)
                else return w
