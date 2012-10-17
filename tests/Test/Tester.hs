{-# LANGUAGE ExistentialQuantification,ImplicitParams #-}
module Test.Tester where

import Data.Encoding
import Data.Encoding.UTF8
import Test.HUnit
import Data.Word
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Test.QuickCheck hiding (Testable)
import Prelude hiding (readFile)
import System.IO.Encoding

data EncodingTest
	= forall enc. (Encoding enc,Show enc) =>
		EncodingTest enc String [Word8]
	| forall enc. (Encoding enc,Show enc) =>
		EncodingFileTest enc FilePath FilePath
	| forall enc. (Encoding enc,Show enc) =>
		DecodingError enc [Word8] DecodingException
	| forall enc. (Encoding enc,Show enc) =>
		EncodingError enc String EncodingException

instance Testable EncodingTest where
    test (EncodingTest enc src trg)
        = TestList
          [TestLabel (show enc ++ " encoding")
           (TestCase $ encodeStrictByteStringExplicit enc src
                         @?= Right (BS.pack trg))
          ,TestLabel (show enc ++ " decoding")
           (TestCase $ decodeStrictByteStringExplicit enc (BS.pack trg)
                         @=? Right src)
          ]
    test (EncodingFileTest e src trg)
        = test $ do
            str_src <- (let ?enc = e in readFile src)
            bsrc <- LBS.readFile src
            str_trg <- (let ?enc = UTF8 in readFile trg)
            str_src @?= str_trg
            --bsrc @=? (encodeLazyByteString enc str_trg)
            
    test (DecodingError enc src ex)
        = TestLabel (show enc ++ " decoding error")
          (TestCase $ decodeStrictByteStringExplicit enc (BS.pack src) @=? Left ex)


charGen :: Gen Char
charGen = let
    ascii = choose (0x00,0x7F) >>= return.chr
    oneByte = choose (0x80,0xFF) >>= return.chr
    twoByte = choose (0x0100,0xFFFF) >>= return.chr
    threeByte = choose (0x010000,0x10FFFF) >>= return.chr
    in frequency [(40,ascii),(30,oneByte),(20,twoByte),(10,threeByte)]

instance Arbitrary Word8 where
    arbitrary = choose (0x00,0xFF::Int) >>= return.fromIntegral

quickCheckEncoding :: Encoding enc => enc -> IO ()
quickCheckEncoding e = do
  quickCheck (encodingIdentity e)
  quickCheck (decodingIdentity e)

encodingIdentity :: Encoding enc => enc -> Property
encodingIdentity e
  = let gen = listOf1 (charGen `suchThat` (encodeable e))
    in forAll gen (\str -> case encodeStrictByteStringExplicit e str of
                      Left err -> property False
                      Right res -> case decodeStrictByteStringExplicit e res of
                        Left err -> property False
                        Right res2 -> property (str==res2))

decodingIdentity :: Encoding enc => enc -> [Word8] -> Property
decodingIdentity e wrd
    = classify (null wrd) "trivial" $ case decoded of
          Left err -> label "trivial" $ property True
          Right res -> case encodeStrictByteStringExplicit e res of
                        Left err -> property False
                        Right res' -> property (bstr==res')
    where
      bstr = BS.pack wrd
      decoded = decodeStrictByteStringExplicit e bstr
