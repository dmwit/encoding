{-# LANGUAGE DeriveDataTypeable #-}
module Data.Encoding.ASCII where

import Control.Throws
import Data.Char
import Data.Encoding.Base
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink
import Data.Encoding.Exception
import Data.Typeable

data ASCII = ASCII deriving (Show,Eq,Typeable)

instance Encoding ASCII where
    decodeChar _ = do
      w <- fetchWord8
      return $ chr $ fromIntegral w
    encodeChar enc c
      | encodeable enc c = pushWord8 . fromIntegral . ord $ c
      | otherwise        = throwException . HasNoRepresentation $ c
    encodeable _ c = c < '\128'
