{-# LANGUAGE ExistentialQuantification,CPP #-}
module Data.Encoding
	(Encoding(..)
	,EncodingException(..)
	,DecodingException(..)
	,recode
	,recodeLazy
	,DynEncoding()
#ifndef USE_HPC
	,encodingFromString
	,encodingFromStringMaybe
#endif
	)
	where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Typeable
import Data.Encoding.Base

#ifndef USE_HPC
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
import Data.Encoding.KOI8U
import Data.Encoding.GB18030
#endif

-- | An untyped encoding. Used in 'System.IO.Encoding.getSystemEncoding'.
data DynEncoding = forall t. (Encoding t,Show t,Typeable t,Eq t)
	=> DynEncoding t 

instance Encoding DynEncoding where
	encode (DynEncoding enc) = encode enc
	encodeLazy (DynEncoding enc) = encodeLazy enc
	encodable (DynEncoding enc) = encodable enc
	decode (DynEncoding enc) = decode enc
	decodeLazy (DynEncoding enc) = decodeLazy enc
	decodable (DynEncoding enc) = decodable enc

instance Show DynEncoding where
	show (DynEncoding enc) = "DynEncoding "++show enc

instance Eq DynEncoding where
	(DynEncoding enc1) == (DynEncoding enc2) = case cast enc2 of
		Nothing -> False
		Just renc2 -> enc1 == renc2

-- | This decodes a string from one encoding and encodes it into another.
recode :: (Encoding from,Encoding to) => from -> to -> ByteString -> ByteString
recode enc_f enc_t bs = encode enc_t (decode enc_f bs)

recodeLazy :: (Encoding from,Encoding to) => from -> to -> Lazy.ByteString -> Lazy.ByteString
recodeLazy enc_f enc_t bs = encodeLazy enc_t (decodeLazy enc_f bs)

#ifndef USE_HPC
-- | Like 'encodingFromString' but returns 'Nothing' instead of throwing an error
encodingFromStringMaybe :: String -> Maybe DynEncoding
encodingFromStringMaybe "ASCII"		= Just $ DynEncoding ASCII
encodingFromStringMaybe "UTF-8"		= Just $ DynEncoding UTF8
encodingFromStringMaybe "UTF-16"	= Just $ DynEncoding UTF16
encodingFromStringMaybe "UTF-32"	= Just $ DynEncoding UTF32
encodingFromStringMaybe "KOI8-R"	= Just $ DynEncoding KOI8R
encodingFromStringMaybe "KOI8-U"	= Just $ DynEncoding KOI8U
encodingFromStringMaybe "ISO-8859-1"	= Just $ DynEncoding ISO88591
encodingFromStringMaybe "ISO-8859-2"	= Just $ DynEncoding ISO88592
encodingFromStringMaybe "ISO-8859-3"	= Just $ DynEncoding ISO88593
encodingFromStringMaybe "ISO-8859-4"	= Just $ DynEncoding ISO88594
encodingFromStringMaybe "ISO-8859-5"	= Just $ DynEncoding ISO88595
encodingFromStringMaybe "ISO-8859-6"	= Just $ DynEncoding ISO88596
encodingFromStringMaybe "ISO-8859-7"	= Just $ DynEncoding ISO88597
encodingFromStringMaybe "ISO-8859-8"	= Just $ DynEncoding ISO88598
encodingFromStringMaybe "ISO-8859-9"	= Just $ DynEncoding ISO88599
encodingFromStringMaybe "ISO-8859-10"	= Just $ DynEncoding ISO885910
encodingFromStringMaybe "ISO-8859-11"	= Just $ DynEncoding ISO885911
encodingFromStringMaybe "ISO-8859-13"	= Just $ DynEncoding ISO885913
encodingFromStringMaybe "ISO-8859-14"	= Just $ DynEncoding ISO885914
encodingFromStringMaybe "ISO-8859-15"	= Just $ DynEncoding ISO885915
encodingFromStringMaybe "ISO-8859-16"	= Just $ DynEncoding ISO885916
encodingFromStringMaybe "CP1250"	= Just $ DynEncoding CP1250
encodingFromStringMaybe "CP1251"	= Just $ DynEncoding CP1251
encodingFromStringMaybe "CP1252"	= Just $ DynEncoding CP1252
encodingFromStringMaybe "CP1253"	= Just $ DynEncoding CP1253
encodingFromStringMaybe "CP1254"	= Just $ DynEncoding CP1254
encodingFromStringMaybe "CP1255"	= Just $ DynEncoding CP1255
encodingFromStringMaybe "CP1256"	= Just $ DynEncoding CP1256
encodingFromStringMaybe "CP1257"	= Just $ DynEncoding CP1257
encodingFromStringMaybe "CP1258"	= Just $ DynEncoding CP1258
encodingFromStringMaybe "GB18030"	= Just $ DynEncoding GB18030
encodingFromStringMaybe _		= Nothing

-- | Takes the name of an encoding and creates a dynamic encoding from it.
encodingFromString :: String -> DynEncoding
encodingFromString str = maybe
	(error $ "Data.Encoding.encodingFromString: Unknown encoding: "++show str)
	id
	(encodingFromStringMaybe str)
#endif
