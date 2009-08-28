{-# LANGUAGE ExistentialQuantification #-}
module Data.Encoding.Base where

import Data.Encoding.Exception
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink

import Control.Throws
import Data.Array.Unboxed as Array
import Data.Map as Map hiding ((!))
import Data.Word
import Data.Char
import Data.Typeable

{- | The base class for all encodings. At least decodeChar, encodeChar and encodeable must be implemented.
 -}
class Encoding enc where
    -- | Read a single character of a ByteSource
    decodeChar :: ByteSource m => enc -> m Char
    -- | Encode a single character and write it to a ByteSink
    encodeChar :: ByteSink m => enc -> Char -> m ()
    -- | Read characters from a ByteSource until it is empty
    decode :: ByteSource m => enc -> m String
    decode e = untilM sourceEmpty (decodeChar e)
    -- | Encode a String and write it to a ByteSink
    encode :: ByteSink m => enc -> String -> m ()
    encode e = mapM_ (encodeChar e)
    -- | Tests whether a given character is representable in the Encoding.
    --   If this yields True, encodeChar must not fail.
    --   If it yields False, encodeChar _must_ throw an exception.
    encodeable :: enc -> Char -> Bool

{- | Wraps all possible encoding types into one data type.
     Used when a function needs to return an encoding.
 -}
data DynEncoding = forall enc. (Encoding enc,Eq enc,Typeable enc,Show enc) => DynEncoding enc

instance Show DynEncoding where
    show (DynEncoding enc) = show enc

instance Encoding DynEncoding where
    decodeChar (DynEncoding e) = decodeChar e
    encodeChar (DynEncoding e) = encodeChar e
    decode (DynEncoding e) = decode e
    encode (DynEncoding e) = encode e
    encodeable (DynEncoding e) = encodeable e

instance Eq DynEncoding where
    (DynEncoding e1) == (DynEncoding e2) = case cast e2 of
                                             Nothing -> False
                                             Just e2' -> e1==e2'

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

encodeableWithMap :: Map Char a -> Char -> Bool
encodeableWithMap = flip Map.member

decodeWithArray :: ByteSource m => UArray Word8 Int -> m Char
decodeWithArray arr = do
  w <- fetchWord8
  let res = arr!w
  if res < 0
    then throwException $ IllegalCharacter w
    else return $ chr res

decodeWithArray2 :: ByteSource m => UArray (Word8,Word8) Int -> m Char
decodeWithArray2 arr = do
  w1 <- fetchWord8
  w2 <- fetchWord8
  if inRange (bounds arr) (w1,w2)
    then (do
           let res = arr!(w1,w2)
           if res < 0
             then throwException $ IllegalCharacter w1
             else return $ chr res
         )
    else throwException $ IllegalCharacter w1