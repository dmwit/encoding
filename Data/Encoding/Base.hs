module Data.Encoding.Base where

import Data.Encoding.Exception
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink

import Control.Throws
import Data.Array as Array
import Data.Map as Map hiding ((!))
import Data.Word
import Data.Char

class Encoding enc where
    decodeChar :: ByteSource m => enc -> m Char
    encodeChar :: ByteSink m => enc -> Char -> m ()
    decode :: ByteSource m => enc -> m String
    decode e = untilM sourceEmpty (decodeChar e)
    encode :: ByteSink m => enc -> String -> m ()
    encode e = mapM_ (encodeChar e)

untilM :: Monad m => m Bool -> m a -> m [a]
untilM check act = do
  end <- check
  if end
    then return []
    else (do
           x <- act
           xs <- untilM check act
           return (x:xs)
           )

untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ check act = untilM check act >> return ()

encodeWithMap :: ByteSink m => Map Char Word8 -> Char -> m ()
encodeWithMap mp c = case Map.lookup c mp of
                       Nothing -> throwException $ HasNoRepresentation c
                       Just v -> pushWord8 v

encodeWithMap2 :: ByteSink m => Map Char (Word8,Word8) -> Char -> m ()
encodeWithMap2 mp c = case Map.lookup c mp of
                        Nothing -> throwException $ HasNoRepresentation c
                        Just (w1,w2) -> do
                          pushWord8 w1
                          pushWord8 w2

decodeWithArray :: ByteSource m => Array Word8 (Maybe Char) -> m Char
decodeWithArray arr = do
  w <- fetchWord8
  case arr!w of
    Nothing -> throwException $ IllegalCharacter w
    Just c -> return c

decodeWithArray2 :: ByteSource m => Array (Word8,Word8) (Maybe Char) -> m Char
decodeWithArray2 arr = do
  w1 <- fetchWord8
  w2 <- fetchWord8
  case arr!(w1,w2) of
    Nothing -> throwException $ IllegalCharacter w1
    Just c -> return c