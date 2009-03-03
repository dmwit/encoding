{-# LANGUAGE DeriveDataTypeable #-}
{- | Implements the japanese character encoding ISO 2022-JP.
     See http://tools.ietf.org/html/rfc1468 for reference.
 -}
module Data.Encoding.ISO2022JP where

import Data.Typeable

import Data.Encoding.Base
import Data.Encoding.Exception
import Data.Encoding.ByteSource
import Data.Encoding.ISO2022
import Data.Encoding.ASCII
import Data.Encoding.JISX0201
import Data.Encoding.JISX0208

import Control.Throws

data ISO2022JP = ISO2022JP deriving (Eq,Show,Typeable)

instance Encoding ISO2022JP where
    encodeChar = encodeCharISO2022
    decodeChar = decodeCharISO2022
    encode = encodeISO2022
    decode = decodeISO2022
    encodeable _ c = encodeable ASCII c || encodeable JISX0201 c || encodeable JISX0208 c

instance ISO2022 ISO2022JP where
    readEscape _ = do
      w <- fetchAhead fetchWord8
      if w == 27
        then (do
               fetchWord8
               w2 <- fetchWord8
               w3 <- fetchWord8
               case w2 of
                 40 -> case w3 of
                        66 -> return $ Just $ DynEncoding ASCII
                        74 -> return $ Just $ DynEncoding JISX0201
                        _ -> throwException (IllegalCharacter w3)
                 36 -> case w3 of
                        64 -> return $ Just $ DynEncoding JISX0208 -- XXX: this actually has to be the 1978 version of the standard... too bad I can't find it
                        66 -> return $ Just $ DynEncoding JISX0208
                        _ -> throwException (IllegalCharacter w3)
                 _ -> throwException (IllegalCharacter w2)
             )
        else return Nothing
    encodingForChar _ c
        | encodeable ASCII c = Just (DynEncoding ASCII,[27,40,66])
        | encodeable JISX0201 c = Just (DynEncoding JISX0201,[27,40,74])
        | encodeable JISX0208 c = Just (DynEncoding JISX0208,[27,36,66])
        | otherwise  = Nothing