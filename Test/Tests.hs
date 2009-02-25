module Test.Tests where

import Test.Tester
import Data.Encoding
import Data.Encoding.UTF8
import Data.Encoding.UTF16
import Data.Encoding.UTF32
import Data.Encoding.ISO88592
import Data.Encoding.ISO88593
import Data.Encoding.ISO88594
import Data.Encoding.ISO88595
import Data.Encoding.ISO88596
import Data.Encoding.ISO88597
import Data.Encoding.ISO88598
import Data.Encoding.ISO88599
import Data.Encoding.ISO885910
import Data.Encoding.ISO885911
import Data.Encoding.ISO885913
import Data.Encoding.ISO885914
import Data.Encoding.JISX0208
import Data.Encoding.ISO2022JP
import Data.Encoding.BootString
import Test.HUnit
import Test.QuickCheck hiding (test)
import Data.Char (ord)

identityTests :: IO ()
identityTests = do
  putStrLn "for UTF8..."
  quickCheckEncoding UTF8Strict
  putStrLn "for UTF16..."
  quickCheckEncoding UTF16BE
  quickCheckEncoding UTF16LE
  putStrLn "for UTF32..."
  quickCheck $ encodingIdentity UTF32
  putStrLn "for ISO88592..."
  quickCheckEncoding ISO88592
  putStrLn "for ISO88593..."
  quickCheckEncoding ISO88593
  putStrLn "for ISO88594..."
  quickCheckEncoding ISO88594
  putStrLn "for ISO88595..."
  quickCheckEncoding ISO88595
  putStrLn "for ISO88596..."
  quickCheckEncoding ISO88596
  putStrLn "for ISO88597..."
  quickCheckEncoding ISO88597
  putStrLn "for ISO88598..."
  quickCheckEncoding ISO88598
  putStrLn "for ISO88599..."
  quickCheckEncoding ISO88599
  putStrLn "for ISO885910..."
  quickCheckEncoding ISO885910
  putStrLn "for ISO885911..."
  quickCheckEncoding ISO885911
  putStrLn "for ISO885913..."
  quickCheckEncoding ISO885913
  putStrLn "for ISO885914..."
  quickCheckEncoding ISO885914
  putStrLn "for Punycode..."
  quickCheck $ encodingIdentity punycode

utf8Tests :: Test
utf8Tests = TestList $ map test $ concat
            [[EncodingTest enc "\x0041\x2262\x0391\x002E"
	                       [0x41,0xE2,0x89,0xA2,0xCE,0x91,0x2E]
	     ,EncodingTest enc "\xD55C\xAD6D\xC5B4"
		               [0xED,0x95,0x9C,0xEA,0xB5,0xAD,0xEC,0x96,0xB4]
	     ,EncodingTest enc "\x65E5\x672C\x8A9E"
		               [0xE6,0x97,0xA5,0xE6,0x9C,0xAC,0xE8,0xAA,0x9E]
	     ,EncodingTest enc "\x233B4"
		               [0xF0,0xA3,0x8E,0xB4]
	     ,EncodingTest enc ""
		               []
	     -- First possible sequence of a certain length
	     ,EncodingTest enc "\x0000"
		               [0x00]
	     ,EncodingTest enc "\x0080"
		               [0xC2,0x80]
	     ,EncodingTest enc "\x0800"
		               [0xE0,0xA0,0x80]
	     ,EncodingTest enc "\x10000"
		               [0xF0,0x90,0x80,0x80]
	     -- Last possible sequence of a certain length
	     ,EncodingTest enc "\x007F"
		               [0x7F]
	     ,EncodingTest enc "\x07FF"
		               [0xDF,0xBF]
	     ,EncodingTest enc "\xFFFF"
		               [0xEF,0xBF,0xBF]
	     -- Other boundaries
	     ,EncodingTest enc "\xD7FF"
		               [0xED,0x9F,0xBF]
	     ,EncodingTest enc "\xE000"
		               [0xEE,0x80,0x80]
	     ,EncodingTest enc "\xFFFD"
		               [0xEF,0xBF,0xBD]
	     -- Illegal starting characters
	     ,DecodingError enc
	                        [0x65,0x55,0x85]
		                (IllegalCharacter 0x85)
	     -- Unexpected end
	     ,DecodingError enc
	                        [0x41,0xE2,0x89,0xA2,0xCE]
		                UnexpectedEnd
	     ,DecodingError enc
		                [0x41,0xE2,0x89]
		                UnexpectedEnd
	     ,DecodingError enc
		                [0x41,0xE2]
		                UnexpectedEnd
             ]
             | enc <- [UTF8,UTF8Strict]
            ]++
	    [DecodingError UTF8 [0xFE] (IllegalCharacter 0xFE)
	    ,DecodingError UTF8 [0xFF] (IllegalCharacter 0xFF)
	    -- Overlong representations of '/'
	    ,DecodingError UTF8Strict [0xC0,0xAF]
		               (IllegalRepresentation [0xC0,0xAF])
	    ,DecodingError UTF8Strict [0xE0,0x80,0xAF]
		               (IllegalRepresentation [0xE0,0x80,0xAF])
	    ,DecodingError UTF8Strict [0xF0,0x80,0x80,0xAF]
		               (IllegalRepresentation [0xF0,0x80,0x80,0xAF])
	    -- Maximum overlong sequences
	    ,DecodingError UTF8Strict [0xC1,0xBF]
		               (IllegalRepresentation [0xC1,0xBF])
	    ,DecodingError UTF8Strict [0xE0,0x9F,0xBF]
		               (IllegalRepresentation [0xE0,0x9F,0xBF])
	    ,DecodingError UTF8Strict [0xF0,0x8F,0xBF,0xBF]
		               (IllegalRepresentation [0xF0,0x8F,0xBF,0xBF])
	    -- Overlong represenations of '\NUL'
	    ,DecodingError UTF8Strict [0xC0,0x80]
		               (IllegalRepresentation [0xC0,0x80])
	    ,DecodingError UTF8Strict [0xE0,0x80,0x80]
		               (IllegalRepresentation [0xE0,0x80,0x80])
	    ,DecodingError UTF8Strict [0xF0,0x80,0x80,0x80]
		               (IllegalRepresentation [0xF0,0x80,0x80,0x80])
	    -- Invalid extends
	    -- 2 of 2
	    ,DecodingError UTF8Strict [0xCC,0x1C,0xE0]
		               (IllegalCharacter 0x1C)
	    -- 2 of 3
	    ,DecodingError UTF8Strict [0xE3,0x6C,0xB3]
		               (IllegalCharacter 0x6C)
	    -- 3 of 3
	    ,DecodingError UTF8Strict [0xE3,0xB4,0x6D]
		               (IllegalCharacter 0x6D)
	    -- 2 of 4
	    ,DecodingError UTF8Strict [0xF2,0x6C,0xB3,0xB3]
		               (IllegalCharacter 0x6C)
	    -- 3 of 4
	    ,DecodingError UTF8Strict [0xF2,0xB3,0x6C,0xB3]
		               (IllegalCharacter 0x6C)
	    -- 4 of 4
	    ,DecodingError UTF8Strict [0xF2,0xB3,0xB3,0x6C]
		               (IllegalCharacter 0x6C)
	    ]

utf16Tests :: Test
utf16Tests = TestList $ map test $
	[EncodingTest UTF16BE "z"
		[0x00,0x7A]
	,EncodingTest UTF16BE "\x6C34"
		[0x6C,0x34]
	,EncodingTest UTF16BE "\x1D11E"
		[0xD8,0x34,0xDD,0x1E]
	,EncodingTest UTF16 "\x6C34z\x1D11E"
		[0xFE,0xFF,0x6C,0x34,0x00,0x7A,0xD8,0x34,0xDD,0x1E]
	,EncodingTest UTF16BE "˨"
		[0x02,0xE8]
	,DecodingError UTF16LE [0x65,0xDC]
		(IllegalCharacter 0xDC)
	,DecodingError UTF16BE [0xDC,0x33]
		(IllegalCharacter 0xDC)
	,DecodingError UTF16BE [0xD9,0x78,0xDA,0x33]
		(IllegalCharacter 0xDA)
	,DecodingError UTF16BE [0xD9,0x78,0xDA,0x66]
		(IllegalCharacter 0xDA)
	]

punycodeTests :: Test
punycodeTests = TestList $ map test $
	[EncodingTest punycode "abcdef"
		(map (fromIntegral.ord) "abcdef-")
	,EncodingTest punycode "abæcdöef"
		(map (fromIntegral.ord) "abcdef-qua4k")
	,EncodingTest punycode "schön"
		(map (fromIntegral.ord) "schn-7qa")
	,EncodingTest punycode "ยจฆฟคฏข"
		(map (fromIntegral.ord) "22cdfh1b8fsa")
	,EncodingTest punycode "☺"
		(map (fromIntegral.ord) "74h")
	-- taken from http://tools.ietf.org/html/rfc3492#section-7
	-- Arabic (Egyptian)
	,punyTest "ليهمابتكلموشعربي؟"
		"egbpdaj6bu4bxfgehfvwxn"
	-- Chinese (simplified)
	,punyTest "他们为什么不说中文"
		"ihqwcrb4cv8a8dqg056pqjye"
	-- Chinese (traditional)
	,punyTest "他們爲什麽不說中文"
		"ihqwctvzc91f659drss3x8bo0yb"
	-- Czech
	,punyTest "Pročprostěnemluvíčesky"
		"Proprostnemluvesky-uyb24dma41a"
	-- Hebrew
	,punyTest "למההםפשוטלאמדבריםעברית"
		"4dbcagdahymbxekheh6e0a7fei0b"
	-- Hindi (Devanagari)
	,punyTest "\x92F\x939\x932\x94B\x917\x939\x93F\x928\x94D\x926\x940\x915\x94D\x92F\x94B\x902\x928\x939\x940\x902\x92C\x94B\x932\x938\x915\x924\x947\x939\x948\x902"
		"i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd"
	-- Japanese (kanji and hiragana)
	,punyTest "なぜみんな日本語を話してくれないのか"
		"n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"
	-- Korean (Hangul syllables)
	,punyTest "세계의모든사람들이한국어를이해한다면얼마나좋을까"
		"989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c"
	-- Russian (Cyrillic)
	,punyTest "почемужеонинеговорятпорусски"
		"b1abfaaepdrnnbgefbadotcwatmq2g4l" -- I think the ietf made a mistake there
	-- Spanish
	,punyTest "PorquénopuedensimplementehablarenEspañol"
		"PorqunopuedensimplementehablarenEspaol-fmd56a"
	-- Vietnamese
	,punyTest "TạisaohọkhôngthểchỉnóitiếngViệt"
		"TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g"
	{-,punyTest "foo\xC2\xAD\xCD\x8F\xE1\xA0\x86\xE1\xA0\x8B\
		\bar\xE2\x80\x8B\xE2\x81\xA0\
		\baz\xEF\xB8\x80\xEF\xB8\x88\xEF\xB8\x8F\xEF\xBB\xBF"
		"foobarbaz"-}
	]
	where punyTest str outp = EncodingTest punycode str (map (fromIntegral.ord) outp)

isoTests :: Test
isoTests = TestList $ map test $
           [EncodingTest ISO88592 "\x104\x2D8\x141\xA4\x13D\x15A\xA7\xA8\x160\x15E\x164\x179\xAD\x17D\x17B\xB0\x105\x2DB\x142\xB4\x13E\x15B\x2C7\xB8\x161\x15F"
            [0xA1..0xBA]
           -- Taken from GNU iconv library
           ,EncodingTest ISO2022JP "Japanese (\x65E5\x672C\x8A9E)\t\t\x3053\x3093\x306B\x3061\x306F\n\tJIS  -- \x5143\x6C17  \x958B\x767A\n"
            [0x4a,0x61,0x70,0x61,0x6e,0x65,0x73,0x65,0x20,0x28,0x1b,0x24,0x42,0x46,0x7c,0x4b
            ,0x5c,0x38,0x6c,0x1b,0x28,0x42,0x29,0x09,0x09,0x1b,0x24,0x42,0x24,0x33,0x24,0x73
            ,0x24,0x4b,0x24,0x41,0x24,0x4f,0x1b,0x28,0x42,0x0a,0x09,0x4a,0x49,0x53,0x20,0x20
            ,0x2d,0x2d,0x20,0x1b,0x24,0x42,0x38,0x35,0x35,0x24,0x1b,0x28,0x42,0x20,0x20,0x1b
            ,0x24,0x42,0x33,0x2b,0x48,0x2f,0x1b,0x28,0x42,0x0a]
           ]

jisTests :: Test
jisTests = TestList $ map test $
           [EncodingTest JISX0208 "\x4E9C"
            [0x30,0x21]
           ]