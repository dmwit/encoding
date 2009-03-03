module Data.Encoding.ISO2022 where

import Data.Encoding.Base
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink
import Data.Encoding.Exception
import Data.Encoding.ASCII

import Data.Word
import Control.Throws

class ISO2022 e where
    readEscape :: ByteSource m => e -> m (Maybe DynEncoding)
    encodingForChar :: e -> Char -> Maybe (DynEncoding,[Word8])

encodeCharISO2022 :: (ISO2022 e,ByteSink m) => e -> Char -> m ()
encodeCharISO2022 e c = case encodingForChar e c of
                          Nothing -> throwException (HasNoRepresentation c)
                          Just (enc,esc) -> do
                                            mapM_ pushWord8 esc
                                            encodeChar enc c

decodeCharISO2022 :: (ISO2022 e,ByteSource m) => e -> m Char
decodeCharISO2022 e = do
  enc <- readEscape e
  case enc of
    Nothing -> decodeChar ASCII
    Just renc -> decodeChar renc

encodeISO2022 :: (ISO2022 e,ByteSink m) => e -> String -> m ()
encodeISO2022 e = encode' (DynEncoding ASCII)
    where
      encode' _ [] = return ()
      encode' enc (c:cs) = case encodingForChar e c of
                             Nothing -> throwException (HasNoRepresentation c)
                             Just (nenc,esc)
                                  | enc==nenc -> do
                                                 encodeChar enc c
                                                 encode' enc cs
                                  | otherwise -> do
                                                 mapM_ pushWord8 esc
                                                 encodeChar nenc c
                                                 encode' nenc cs

decodeISO2022 :: (ISO2022 e,ByteSource m) => e -> m String
decodeISO2022 e = decode' (DynEncoding ASCII)
    where
      decode' enc = do
        empty <- sourceEmpty
        if empty
          then return []
          else (do
                 nenc <- readEscape e
                 case nenc of
                   Just renc -> decode' renc
                   Nothing -> do
                             c <- decodeChar enc
                             cs <- decode' enc
                             return (c:cs)
               )
