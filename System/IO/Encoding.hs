{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImplicitParams           #-}
{- | This module provides a replacement for the normal (unicode unaware) IO functions of haskell.
     By using implicit parameters, it can be used almost as a drop-in replacement.
     For example, consider the following simple echo program:

     > main = do
     >   str <- getContents
     >   putStr str

     To make this program process UTF-8 data, change the program to:

     > {-# LANGUAGE ImplicitParams #-}
     >
     > import Prelude hiding (getContents,putStr)
     > import System.IO.Encoding
     > import Data.Encoding.UTF8
     >
     > main = do
     >   let ?enc = UTF8
     >   str <- getContents
     >   putStr str

     Or, if you want to use the standard system encoding:

     > {-# LANGUAGE ImplicitParams #-}
     >
     > import Prelude hiding (getContents,putStr)
     > import System.IO.Encoding
     >
     > main = do
     >   e <- getSystemEncoding
     >   let ?enc = e
     >   str <- getContents
     >   putStr str
 -}
module System.IO.Encoding
    (getSystemEncoding
    ,getContents
    ,putStr
    ,putStrLn
    ,hPutStr
    ,hPutStrLn
    ,hGetContents
    ,readFile
    ,writeFile
    ,appendFile
    ,getChar
    ,hGetChar
    ,getLine
    ,hGetLine
    ,putChar
    ,hPutChar
    ,interact
    ,print
    ,hPrint) where

import           Foreign.C.String

import           Control.Monad.Reader (runReaderT)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Encoding
import           Prelude              hiding (appendFile, getChar, getContents,
                                       getLine, interact, print, putChar,
                                       putStr, putStrLn, readFile, writeFile)
import           System.IO            (Handle, stdin, stdout)

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

putStr :: (Encoding e,?enc :: e) => String -> IO ()
putStr = hPutStr stdout

putStrLn :: (Encoding e,?enc :: e) => String -> IO ()
putStrLn = hPutStrLn stdout

-- | Like the normal 'System.IO.hPutStr', but encodes the output using an
--   encoding.
hPutStr :: (Encoding e,?enc :: e) => Handle -> String -> IO ()
hPutStr h str = LBS.hPut h (encodeLazyByteString ?enc str)

hPutStrLn :: (Encoding e,?enc :: e) => Handle -> String -> IO ()
hPutStrLn h str = do
    LBS.hPut h (encodeLazyByteString ?enc str)
    LBS.hPut h (encodeLazyByteString ?enc "\n")

print :: (Encoding e,Show a,?enc :: e) => a -> IO ()
print = hPrint stdout

hPrint :: (Encoding e,Show a,?enc :: e) => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)

readFile :: (Encoding e,?enc :: e) => FilePath -> IO String
readFile fn = LBS.readFile fn >>= return.(decodeLazyByteString ?enc)

writeFile :: (Encoding e,?enc :: e) => FilePath -> String -> IO ()
writeFile fn str = LBS.writeFile fn $ encodeLazyByteString ?enc str

appendFile :: (Encoding e,?enc :: e) => FilePath -> String -> IO ()
appendFile fn str = LBS.appendFile fn $ encodeLazyByteString ?enc str

getChar :: (Encoding e,?enc :: e) => IO Char
getChar = hGetChar stdin

hGetChar :: (Encoding e,?enc :: e) => Handle -> IO Char
hGetChar h = runReaderT (decodeChar ?enc) h

getLine :: (Encoding e,?enc :: e) => IO String
getLine = hGetLine stdin

hGetLine :: (Encoding e,?enc :: e) => Handle -> IO String
hGetLine h = do
  line <- BS.hGetLine h
  return $ decodeStrictByteString ?enc line

putChar :: (Encoding e,?enc :: e) => Char -> IO ()
putChar = hPutChar stdout

hPutChar :: (Encoding e,?enc :: e) => Handle -> Char -> IO ()
hPutChar h c = runReaderT (encodeChar ?enc c) h

interact :: (Encoding e,?enc :: e) => (String -> String) -> IO ()
interact f = do
  line <- hGetLine stdin
  hPutStrLn stdout (f line)

#ifndef mingw32_HOST_OS
foreign import ccall "system_encoding.h get_system_encoding"
	get_system_encoding :: IO CString
#endif

-- | On unix machines, returns the system's currently configured text encoding,
-- or Nothing if there was an error. On Windows, currently always returns Nothing.
getSystemEncoding :: IO (Maybe DynEncoding)
getSystemEncoding = do
#ifndef mingw32_HOST_OS
  enc <- get_system_encoding
  str <- peekCString enc
  return $ encodingFromStringExplicit str
#else
  return Nothing
#endif
