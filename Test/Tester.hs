{-# LANGUAGE ExistentialQuantification #-}
module Test.Tester where

import Data.Encoding
import Test.HUnit
import Data.Word
import Data.ByteString (pack)
import Control.Exception (catchDyn,evaluate)

data EncodingTest
	= forall enc. (Encoding enc,Show enc) =>
		EncodingTest enc String [Word8]
	| forall enc. (Encoding enc,Show enc) =>
		DecodingError enc [Word8] DecodingException

instance Testable EncodingTest where
	test (EncodingTest enc src trg) = TestList
		[TestLabel (show enc ++ " encodable")
			(TestCase $ (all (encodable enc) src) @=? True)
		,TestLabel (show enc ++ " encoding")
			(TestCase $ (encode enc src) @=? bstr)
		,TestLabel (show enc ++ " decodable")
			(TestCase $ (decodable enc bstr) @=? True)
		,TestLabel (show enc ++ " decoding")
			(TestCase $ (decode enc bstr) @=? src)
		]
		where
		bstr = pack trg
	test (DecodingError enc trg what) = TestList
		[TestLabel (show enc ++ " not decodable") $
			TestCase $ assert $ not $ decodable enc (pack trg)
		,TestLabel (show enc ++ " decoding error") $ TestCase $ 
			catchDyn (do
				evaluate (decode enc (pack trg) == "")
				return ())
				(\exc -> exc @=? what)
		]
