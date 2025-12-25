{-# LANGUAGE FlexibleInstances,FlexibleContexts,MultiParamTypeClasses,CPP #-}
module Data.Encoding.ByteSink where

import Data.Encoding.Exception

import Data.Binary.Put
import Data.Bits
import Data.Char
import Data.Sequence
import Data.Word
import Data.Foldable (toList)
import Control.Throws
import Control.Exception
import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, modify)
import Control.Monad.Reader (ReaderT, ask)
import Foreign.Ptr (Ptr,plusPtr,minusPtr)
import Foreign.Marshal.Alloc (mallocBytes,reallocBytes,free)
import Foreign.Storable (poke)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)

class (Monad m,Throws EncodingException m) => ByteSink m where
    pushWord8 :: Word8 -> m ()
    pushWord16be :: Word16 -> m ()
    pushWord16be w = do
      pushWord8 (fromIntegral $ w `shiftR` 8)
      pushWord8 (fromIntegral $ w)
    pushWord16le :: Word16 -> m ()
    pushWord16le w = do
      pushWord8 (fromIntegral $ w)
      pushWord8 (fromIntegral $ w `shiftR` 8)
    pushWord32be :: Word32 -> m ()
    pushWord32be w = do
      pushWord8 (fromIntegral $ w `shiftR` 24)
      pushWord8 (fromIntegral $ w `shiftR` 16)
      pushWord8 (fromIntegral $ w `shiftR`  8)
      pushWord8 (fromIntegral $ w)
    pushWord32le :: Word32 -> m ()
    pushWord32le w = do
      pushWord8 (fromIntegral $ w)
      pushWord8 (fromIntegral $ w `shiftR`  8)
      pushWord8 (fromIntegral $ w `shiftR` 16)
      pushWord8 (fromIntegral $ w `shiftR` 24)
    pushWord64be :: Word64 -> m ()
    pushWord64be w = do
      pushWord8 (fromIntegral $ w `shiftR` 56)
      pushWord8 (fromIntegral $ w `shiftR` 48)
      pushWord8 (fromIntegral $ w `shiftR` 40)
      pushWord8 (fromIntegral $ w `shiftR` 32)
      pushWord8 (fromIntegral $ w `shiftR` 24)
      pushWord8 (fromIntegral $ w `shiftR` 16)
      pushWord8 (fromIntegral $ w `shiftR`  8)
      pushWord8 (fromIntegral $ w)
    pushWord64le :: Word64 -> m ()
    pushWord64le w = do
      pushWord8 (fromIntegral $ w)
      pushWord8 (fromIntegral $ w `shiftR`  8)
      pushWord8 (fromIntegral $ w `shiftR` 16)
      pushWord8 (fromIntegral $ w `shiftR` 24)
      pushWord8 (fromIntegral $ w `shiftR` 32)
      pushWord8 (fromIntegral $ w `shiftR` 40)
      pushWord8 (fromIntegral $ w `shiftR` 48)
      pushWord8 (fromIntegral $ w `shiftR` 56)

instance Throws EncodingException PutM where
    throwException = throw

instance ByteSink PutM where
    pushWord8 = putWord8
    pushWord16be = putWord16be
    pushWord16le = putWord16le
    pushWord32be = putWord32be
    pushWord32le = putWord32le
    pushWord64be = putWord64be
    pushWord64le = putWord64le

newtype PutME a = PutME (Either EncodingException (PutM (),a))

instance Functor PutME where
  fmap = liftM

instance Applicative PutME where
  pure x = PutME $ Right (pure (),x)
  (<*>) = ap

instance Monad PutME where
    return = pure
    (PutME x) >>= g = PutME $ do
                        (m,r) <- x
                        let (PutME ng) = g r
                        case ng of
                          Left err -> Left err
                          Right (m',nr) -> Right (m>>m',nr)

instance Throws EncodingException PutME where
    throwException = PutME . Left

instance ByteSink PutME where
    pushWord8 w = PutME $ Right (putWord8 w,())
    pushWord16be w = PutME $ Right (putWord16be w,())
    pushWord16le w = PutME $ Right (putWord16le w,())
    pushWord32be w = PutME $ Right (putWord32be w,())
    pushWord32le w = PutME $ Right (putWord32le w,())
    pushWord64be w = PutME $ Right (putWord64be w,())
    pushWord64le w = PutME $ Right (putWord64le w,())

#if MIN_VERSION_base(4,3,0)
#else
instance Monad (Either EncodingException) where
    return x = Right x
    Left err >>= g = Left err
    Right x >>= g = g x
#endif

instance (Monad m,Throws EncodingException m) => ByteSink (StateT (Seq Char) m) where
    pushWord8 x = modify (|> (chr $ fromIntegral x))

newtype StrictSink a = StrictS (Ptr Word8 -> Int -> Int -> IO (a,Ptr Word8,Int,Int))

instance Functor StrictSink where
  fmap = liftM

instance Applicative StrictSink where
  pure x = StrictS $ \cstr pos max -> return (x,cstr,pos,max)
  (<*>) = ap

instance Monad StrictSink where
    return = pure
    (StrictS f) >>= g = StrictS (\cstr pos max -> do
                                   (res,ncstr,npos,nmax) <- f cstr pos max
                                   let StrictS g' = g res
                                   g' ncstr npos nmax
                                )

instance Throws EncodingException StrictSink where
    throwException = throw

instance ByteSink StrictSink where
    pushWord8 x = StrictS (\cstr pos max -> do
                             (ncstr,nmax) <- if pos < max
                                    then return (cstr,max)
                                    else (do
                                           let nmax = max + 32
                                           nptr <- reallocBytes cstr nmax
                                           return (nptr,nmax)
                                           )
                             poke (ncstr `plusPtr` pos) x
                             return ((),ncstr,pos+1,nmax)
                          )

newtype StrictSinkE a = StrictSinkE (StrictSink (Either EncodingException a))

instance Functor StrictSinkE where
  fmap = liftM

instance Applicative StrictSinkE where
  pure = StrictSinkE . return . Right
  (<*>) = ap

instance Monad StrictSinkE where
    return = pure
    (StrictSinkE s) >>= g = StrictSinkE $ do
                              res <- s
                              case res of
                                Left err -> return $ Left err
                                Right res' -> let StrictSinkE g' = g res'
                                             in g'

instance Throws EncodingException StrictSinkE where
    throwException = StrictSinkE . return . Left

instance ByteSink StrictSinkE where
    pushWord8 x = StrictSinkE $ pushWord8 x >>= return . Right

createStrictWithLen :: StrictSink a -> Int -> (a,BS.ByteString)
createStrictWithLen (StrictS f) max = unsafePerformIO $ do
                                 ptr <- mallocBytes max
                                 (r,nptr,len,_) <- f ptr 0 max
                                 str <- unsafePackCStringFinalizer nptr len (free nptr)
                                 return (r,str)

createStrict :: StrictSink a -> (a,BS.ByteString)
createStrict sink = createStrictWithLen sink 32

newtype StrictSinkExplicit a = StrictSinkExplicit  (StrictSink (Either EncodingException a))

instance Functor StrictSinkExplicit where
  fmap = liftM

instance Applicative StrictSinkExplicit where
  pure = (StrictSinkExplicit).return.Right
  (<*>) = ap

instance Monad StrictSinkExplicit where
    return = pure
    (StrictSinkExplicit sink) >>= f
        = StrictSinkExplicit (do
                               res <- sink
                               case res of
                                 Left err -> return $ Left err
                                 Right x -> let StrictSinkExplicit sink2 = f x
                                           in sink2)

instance Throws EncodingException StrictSinkExplicit where
    throwException = StrictSinkExplicit . return . Left

instance ByteSink StrictSinkExplicit where
    pushWord8 x = StrictSinkExplicit $ do
                                       pushWord8 x
                                       return $ Right ()

instance ByteSink (ReaderT Handle IO) where
    pushWord8 x = do
      h <- ask
      liftIO $ do
        hPutChar h (chr $ fromIntegral x)
