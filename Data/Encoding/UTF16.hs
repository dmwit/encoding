{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements UTF-16 encoding and decoding as in RFC 2781.
     See <http://en.wikipedia.org/wiki/UTF-16> for more information.
 -}
module Data.Encoding.UTF16
    (UTF16(..)
    ) where

import Data.Encoding.Base
import Data.Encoding.ByteSink
import Data.Encoding.ByteSource
import Data.Encoding.Exception

import Control.Throws
import Data.Bits
import Data.Char
import Data.Typeable
import Data.Word

data UTF16
    = UTF16	-- ^ Decodes big and little endian, encodes big endian.
    | UTF16BE	-- ^ Big endian decoding and encoding, fails if the string isn\'t actually big endian.
    | UTF16LE	-- ^ Little endian decoding and encoding.
      deriving (Eq,Show,Typeable)

readBOM :: ByteSource m => m (Either Char UTF16)
readBOM = do
  ch <- decodeChar UTF16
  case ch of
    '\xFEFF' -> return (Right UTF16BE)
    '\xFFFE' -> return (Right UTF16LE)
    _ -> return (Left ch)
  
decodeUTF16 :: ByteSource m => (m Word16) -> m Char
decodeUTF16 fetch = do
  w1 <- fetch
  if w1 < 0xD800 || w1 > 0xDFFF
    then return (chr $ fromIntegral w1)
    else (if w1 > 0xDBFF
            then throwException (IllegalCharacter (fromIntegral (w1 `shiftR` 8)))
            else (do
                   w2 <- fetch
                   if w2 < 0xDC00 || w2 > 0xDFFF
                     then throwException (IllegalCharacter (fromIntegral (w2 `shiftR` 8)))
                     else let v = ((fromIntegral (w1 .&. 0x3FF)) `shiftL` 10)
                                  .|. (fromIntegral (w2 .&. 0x3FF))
                          in return $ chr (v+0x10000)
                 )
         )

encodeUTF16 :: ByteSink m => (Word16 -> m ()) -> Char -> m ()
encodeUTF16 push ch
    | val<=0xDFFF && val>=0xD800 = throwException (HasNoRepresentation ch)
    | val<=0x0000FFFF = push $ fromIntegral val
    | val<=0x0010FFFF = let v = val - 0x10000
                            w1 = (fromIntegral (v `shiftR` 10)) .|. 0xD800
                            w2 = ((fromIntegral v) .&. 0x3FF) .|. 0xDC00
                        in push w1 >> push w2
    | otherwise = throwException (HasNoRepresentation ch)
    where
      val = ord ch

instance Encoding UTF16 where
    encodeChar UTF16LE = encodeUTF16 pushWord16le
    encodeChar _ = encodeUTF16 pushWord16be
    decodeChar UTF16LE = decodeUTF16 fetchWord16le
    decodeChar _ = decodeUTF16 fetchWord16be

    encode UTF16 str = do
      encodeChar UTF16 '\xFEFF'
      mapM_ (encodeChar UTF16) str
    encode enc str = mapM_ (encodeChar enc) str

    decode UTF16 = do
      res <- readBOM
      case res of
        Left c -> do
                 cs <- untilM sourceEmpty (decodeChar UTF16BE)
                 return (c:cs)
        Right bom -> decode bom
    decode enc = untilM sourceEmpty (decodeChar enc)
