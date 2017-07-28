{-# LANGUAGE DeriveDataTypeable #-}
module Data.Encoding.Exception where

import Control.Exception.Extensible
import Data.Word
import Data.Typeable
import Control.Monad.Identity

-- | This exception type is thrown whenever something went wrong during the
--   encoding-process.
data EncodingException
    = HasNoRepresentation Char  -- ^ Thrown if a specific character
                                --   is not representable in an encoding.
    deriving (Eq,Ord,Show,Read,Typeable)

instance Exception EncodingException

-- | This exception type is thrown whenever something went wrong during the
--   decoding-process.
data DecodingException
    = IllegalCharacter Word8        -- ^ The sequence contained an illegal
                                    --   byte that couldn't be decoded.
    | UnexpectedEnd                 -- ^ more bytes were needed to allow a
                                    --   successfull decoding.
    | OutOfRange                    -- ^ the decoded value was out of the unicode range
    | IllegalRepresentation [Word8] -- ^ The character sequence encodes a
                                    --   character, but is illegal.
    deriving (Eq,Ord,Show,Read,Typeable)

instance Exception DecodingException
