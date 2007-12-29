{-# LANGUAGE ExistentialQuantification #-}
module Data.Encoding
	(Encoding(..)
	,EncodingException(..)
	,DecodingException(..)
	,recode
	,recodeLazy
	,DynEncoding()
	,encodingFromString
	)
	where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Encoding.Base
import Data.Encoding.ASCII
import Data.Encoding.UTF8
import Data.Encoding.UTF16
import Data.Encoding.UTF32
import Data.Encoding.ISO88591
import Data.Encoding.ISO88592
import Data.Encoding.ISO88593
import Data.Encoding.ISO88594
import Data.Encoding.ISO88595
import Data.Encoding.ISO88596
import Data.Encoding.ISO88597
import Data.Encoding.ISO88598
import Data.Encoding.ISO88599
import Data.Encoding.ISO885910
import Data.Encoding.ISO885911
import Data.Encoding.ISO885913
import Data.Encoding.ISO885914
import Data.Encoding.ISO885915
import Data.Encoding.ISO885916
import Data.Encoding.CP1250
import Data.Encoding.CP1251
import Data.Encoding.CP1252
import Data.Encoding.CP1253
import Data.Encoding.CP1254
import Data.Encoding.CP1255
import Data.Encoding.CP1256
import Data.Encoding.CP1257
import Data.Encoding.CP1258
import Data.Encoding.KOI8R
import Data.Encoding.GB18030

-- | An untyped encoding. Used in 'System.IO.Encoding.getSystemEncoding'.
data DynEncoding = forall t. (Encoding t,Show t) => DynEncoding t 

instance Encoding DynEncoding where
	encode (DynEncoding enc) = encode enc
	encodeLazy (DynEncoding enc) = encodeLazy enc
	encodable (DynEncoding enc) = encodable enc
	decode (DynEncoding enc) = decode enc
	decodeLazy (DynEncoding enc) = decodeLazy enc
	decodable (DynEncoding enc) = decodable enc

instance Show DynEncoding where
	show (DynEncoding enc) = "DynEncoding "++show enc

-- | This decodes a string from one encoding and encodes it into another.
recode :: (Encoding from,Encoding to) => from -> to -> ByteString -> ByteString
recode enc_f enc_t bs = encode enc_t (decode enc_f bs)

recodeLazy :: (Encoding from,Encoding to) => from -> to -> Lazy.ByteString -> Lazy.ByteString
recodeLazy enc_f enc_t bs = encodeLazy enc_t (decodeLazy enc_f bs)

-- | Takes the name of an encoding and creates a dynamic encoding from it.
encodingFromString :: String -> DynEncoding
encodingFromString "ASCII"	= DynEncoding ASCII
encodingFromString "UTF-8"	= DynEncoding UTF8
encodingFromString "UTF-16"	= DynEncoding UTF16
encodingFromString "UTF-32"	= DynEncoding UTF32
encodingFromString "KOI8-R"	= DynEncoding KOI8R
encodingFromString "ISO-8859-1"	= DynEncoding ISO88591
encodingFromString "ISO-8859-2"	= DynEncoding ISO88592
encodingFromString "ISO-8859-3"	= DynEncoding ISO88593
encodingFromString "ISO-8859-4"	= DynEncoding ISO88594
encodingFromString "ISO-8859-5"	= DynEncoding ISO88595
encodingFromString "ISO-8859-6"	= DynEncoding ISO88596
encodingFromString "ISO-8859-7"	= DynEncoding ISO88597
encodingFromString "ISO-8859-8"	= DynEncoding ISO88598
encodingFromString "ISO-8859-9"	= DynEncoding ISO88599
encodingFromString "ISO-8859-10"= DynEncoding ISO885910
encodingFromString "ISO-8859-11"= DynEncoding ISO885911
encodingFromString "ISO-8859-13"= DynEncoding ISO885913
encodingFromString "ISO-8859-14"= DynEncoding ISO885914
encodingFromString "ISO-8859-15"= DynEncoding ISO885915
encodingFromString "ISO-8859-16"= DynEncoding ISO885916
encodingFromString "CP1250"	= DynEncoding CP1250
encodingFromString "CP1251"	= DynEncoding CP1251
encodingFromString "CP1252"	= DynEncoding CP1252
encodingFromString "CP1253"	= DynEncoding CP1253
encodingFromString "CP1254"	= DynEncoding CP1254
encodingFromString "CP1255"	= DynEncoding CP1255
encodingFromString "CP1256"	= DynEncoding CP1256
encodingFromString "CP1257"	= DynEncoding CP1257
encodingFromString "CP1258"	= DynEncoding CP1258
encodingFromString "GB18030"	= DynEncoding GB18030
encodingFromString str		= error $ "Unknown encoding: "++show str
