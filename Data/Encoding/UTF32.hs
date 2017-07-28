{-# LANGUAGE DeriveDataTypeable #-}
{- | This module implements UTF-32 encoding and decoding.
     See <http://en.wikipedia.org/wiki/UTF-32> for more information.
 -}
module Data.Encoding.UTF32
    (UTF32(..))
    where

import Data.Encoding.Base
import Data.Encoding.ByteSink
import Data.Encoding.ByteSource
import Data.Encoding.Exception

import Data.Char
import Data.Typeable


data UTF32
    = UTF32      -- ^ Detects big or little endian through the use of the BOM (Byte Order Mask) character. Defaults to big endian if not present.
    | UTF32BE    -- ^ Encodes and decodes using the big endian encoding.
    | UTF32LE    -- ^ Encodes and decodes using the little endian encoding.
    deriving (Eq,Show,Typeable)

instance Encoding UTF32 where
    encodeChar UTF32LE ch = pushWord32le (fromIntegral $ ord ch)
    encodeChar _ ch = pushWord32be (fromIntegral $ ord ch)
    decodeChar UTF32LE = do
                          wrd <- fetchWord32le
                          return $ chr $ fromIntegral wrd
    decodeChar _ = do
                          wrd <- fetchWord32be
                          return $ chr $ fromIntegral wrd
    encode UTF32 str = do
      encodeChar UTF32 '\xFEFF'
      mapM_ (encodeChar UTF32) str
    encode enc str = mapM_ (encodeChar enc) str

    decode UTF32 = do
      ch <- fetchWord32be
      case ch of
        0x0000FEFF -> untilM sourceEmpty (decodeChar UTF32BE)
        0xFFFE0000 -> untilM sourceEmpty (decodeChar UTF32LE)
        _ -> do
          rest <- untilM sourceEmpty (decodeChar UTF32)
          return ((chr $ fromIntegral ch):rest)
    decode enc = untilM sourceEmpty (decodeChar enc)
    encodeable _ _ = True
