module Data.Encoding.ASCII
	(ASCII(..)) where

import Control.Exception (throwDyn)
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as Lazy (pack)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Base (c2w)
import qualified Data.ByteString.Lazy as Lazy
import Data.Encoding.Base
import Data.Word

data ASCII = ASCII

charToASCII :: Char -> Word8
charToASCII ch = if ch < '\128'
	then c2w ch
	else throwDyn (HasNoRepresentation ch)

instance Encoding ASCII where
	encode _ str = pack (map charToASCII str)
	encodeLazy _ str = Lazy.pack (map charToASCII str)
	encodable _ ch = ch < '\128'
	decode _ = unpack
	decodable _ = const True
