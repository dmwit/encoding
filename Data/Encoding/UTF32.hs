module Data.Encoding.UTF32
	(UTF32(..))
	where

import Data.Bits
import Data.Char (ord,chr)
import Data.Encoding.Base
import Data.Word
import Control.Exception (throwDyn)

data UTF32 = UTF32

instance Encoding UTF32 where
	encode _ = encodeMultibyte encodeUTF32
	encodeLazy _ = encodeMultibyteLazy encodeUTF32
	encodable _ c = ord c < 0x0010FFFF
	decode _ = decodeMultibyte decodeUTF32
	decodeLazy _ = decodeMultibyteLazy decodeUTF32

encodeUTF32 :: Char -> (Word8,EncodeState)
encodeUTF32 ch = let
	w  = ord ch	
	w1 = fromIntegral $ w `shiftR` 24
	w2 = fromIntegral $ w `shiftR` 16
	w3 = fromIntegral $ w `shiftR`  8
	w4 = fromIntegral $ w
	in (w1,Put3 w2 w3 w4)

decodeUTF32 :: [Word8] -> (Char,[Word8])
decodeUTF32 (w1:w2:w3:w4:rest) = (chr $
	(fromIntegral w1 `shiftL` 24) .|.
	(fromIntegral w2 `shiftL` 16) .|.
	(fromIntegral w3 `shiftL`  8) .|.
	(fromIntegral w4),rest)
decodeUTF32 _ = throwDyn UnexpectedEnd
	
