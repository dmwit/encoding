{-# LANGUAGE ImplicitParams,ForeignFunctionInterface #-}
{- | This module provides a replacement for the normal (unicode unaware) IO functions of haskell.
     By using implicit parameters, it can be used almost as a drop-in replacement.
 -}
module System.IO.Encoding
    (getSystemEncoding
    ,getContents
    ,hPutStr
    ,hPutStrLn
    ,hGetContents
    ,readFile
    ,writeFile
    ,appendFile
    ,hGetChar
    ,hGetLine
    ,hPutChar
    ,interact
    ,print) where

import Foreign.C.String

import Data.Encoding
import System.IO (Handle,stdout,stdin)
import Prelude hiding (print,getContents,readFile,writeFile,appendFile,interact)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Monad.Reader (runReaderT)

-- | Like the normal 'System.IO.hGetContents', but decodes the input using an
--   encoding.
hGetContents :: (Encoding e,?enc :: e) => Handle -> IO String
hGetContents h = do
	str <- LBS.hGetContents h
	return $ decodeLazyByteString ?enc str

getContents :: (Encoding e,?enc :: e) => IO String
getContents = do
    str <- LBS.getContents
    return $ decodeLazyByteString ?enc str

-- | Like the normal 'System.IO.hPutStr', but encodes the output using an
--   encoding.
hPutStr :: (Encoding e,?enc :: e) => Handle -> String -> IO ()
hPutStr h str = LBS.hPut h (encodeLazyByteString ?enc str)

hPutStrLn :: (Encoding e,?enc :: e) => Handle -> String -> IO ()
hPutStrLn h str = do
    LBS.hPut h (encodeLazyByteString ?enc str)
    LBS.hPut h (encodeLazyByteString ?enc "\n")

print :: (Encoding e,Show a,?enc :: e) => a -> IO ()
print x = hPutStrLn stdout (show x)

readFile :: (Encoding e,?enc :: e) => FilePath -> IO String
readFile fn = LBS.readFile fn >>= return.(decodeLazyByteString ?enc)

writeFile :: (Encoding e,?enc :: e) => FilePath -> String -> IO ()
writeFile fn str = LBS.writeFile fn $ encodeLazyByteString ?enc str

appendFile :: (Encoding e,?enc :: e) => FilePath -> String -> IO ()
appendFile fn str = LBS.appendFile fn $ encodeLazyByteString ?enc str

hGetChar :: (Encoding e,?enc :: e) => Handle -> IO Char
hGetChar h = runReaderT (decodeChar ?enc) h

hGetLine :: (Encoding e,?enc :: e) => Handle -> IO String
hGetLine h = do
  line <- BS.hGetLine h
  return $ decodeStrictByteString ?enc line

hPutChar :: (Encoding e,?enc :: e) => Handle -> Char -> IO ()
hPutChar h c = runReaderT (encodeChar ?enc c) h

interact :: (Encoding e,?enc :: e) => (String -> String) -> IO ()
interact f = do
  line <- hGetLine stdin
  hPutStrLn stdout (f line)

foreign import ccall "system_encoding.h get_system_encoding"
	get_system_encoding :: IO CString

-- | Returns the encoding used on the current system.
getSystemEncoding :: IO DynEncoding
getSystemEncoding = do
	enc <- get_system_encoding
	str <- peekCString enc
	return $ encodingFromString str