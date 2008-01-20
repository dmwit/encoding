module Test.Tests where

import Data.Encoding
import Data.Encoding.ASCII
import Data.Encoding.UTF8
import Data.Encoding.UTF16
import Data.Encoding.BootString
import Test.Tester
import Test.HUnit
import Data.Char (ord)

asciiTests :: Test
asciiTests = TestList $ map test $
	[EncodingTest ASCII
		"Hello, world!"
		[0x48,0x65,0x6C,0x6C,0x6F,0x2C,0x20,0x77,0x6F,0x72,0x6C,0x64,0x21]
	,EncodingError ASCII
		"\x0041\x2262\x0391\x002E"
		(HasNoRepresentation '\x2262')
	]

utf8Tests :: Test
utf8Tests = TestList $ map test $
	-- Simple encoding tests
	concat [[EncodingTest enc "\x0041\x2262\x0391\x002E"
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
		UnexpectedEnd]
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
	,DecodingError UTF16BE [0xDC]
		(IllegalCharacter 0xDC)
	,DecodingError UTF16BE [0xD9,0x78,0xDA]
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
	,punyTest "\x644\x64a\x647\x645\x627\x628\x62a\x643\x644\x645\x648\x634\x639\x631\x628\x64a\x61f"
		"egbpdaj6bu4bxfgehfvwxn"
	-- Chinese (simplified)
	,punyTest "\x4ed6\x4eec\x4e3a\x4ec0\x4e48\x4e0d\x8bf4\x4e2d\x6587"
		"ihqwcrb4cv8a8dqg056pqjye"
	-- Chinese (traditional)
	,punyTest "\x4ed6\x5011\x7232\x4ec0\x9ebd\x4e0d\x8aaa\x4e2d\x6587"
		"ihqwctvzc91f659drss3x8bo0yb"
	-- Czech
	,punyTest "\x050\x072\x06f\x10d\x070\x072\x06f\x073\x074\x11b\x06e\x065\x06d\x06c\x075\x076\x0ed\x10d\x065\x073\x06b\x079"
		"Proprostnemluvesky-uyb24dma41a"
	-- Hebrew
	,punyTest "\x5dc\x5de\x5d4\x5d4\x5dd\x5e4\x5e9\x5d5\x5d8\x5dc\x5d0\x5de\x5d3\x5d1\x5e8\x5d9\x5dd\x5e2\x5d1\x5e8\x5d9\x5ea"
		"4dbcagdahymbxekheh6e0a7fei0b"
	-- Hindi (Devanagari)
	,punyTest "\x92F\x939\x932\x94B\x917\x939\x93F\x928\x94D\x926\x940\x915\x94D\x92F\x94B\x902\x928\x939\x940\x902\x92C\x94B\x932\x938\x915\x924\x947\x939\x948\x902"
		"i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd"
	-- Japanese (kanji and hiragana)
	,punyTest "\x306A\x305C\x307F\x3093\x306A\x65E5\x672C\x8A9E\x3092\x8A71\x3057\x3066\x304F\x308C\x306A\x3044\x306E\x304B"
		"n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"
	-- Korean (Hangul syllables)
	,punyTest "\xC138\xACC4\xC758\xBAA8\xB4E0\xC0AC\xB78C\xB4E4\xC774\xD55C\xAD6D\xC5B4\xB97C\xC774\xD574\xD55C\xB2E4\xBA74\xC5BC\xB9C8\xB098\xC88B\xC744\xAE4C"
		"989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c"
	-- Russian (Cyrillic)
	,punyTest "\x43F\x43E\x447\x435\x43C\x443\x436\x435\x43E\x43D\x438\x43D\x435\x433\x43E\x432\x43E\x440\x44F\x442\x43F\x43E\x440\x443\x441\x441\x43A\x438"
		"b1abfaaepdrnnbgefbadotcwatmq2g4l" -- I think the ietf made a mistake there
	-- Spanish
	,punyTest "\x50\x6F\x72\x71\x75\xE9\x6E\x6F\x70\x75\x65\x64\x65\x6E\x73\x69\x6D\x70\x6C\x65\x6D\x65\x6E\x74\x65\x68\x61\x62\x6C\x61\x72\x65\x6E\x45\x73\x70\x61\xF1\x6F\x6C"
		"PorqunopuedensimplementehablarenEspaol-fmd56a"
	-- Vietnamese
	,punyTest "\x0054\x1EA1\x0069\x0073\x0061\x006F\x0068\x1ECD\x006B\x0068\x00F4\x006E\x0067\x0074\x0068\x1EC3\x0063\x0068\x1EC9\x006E\x00F3\x0069\x0074\x0069\x1EBF\x006E\x0067\x0056\x0069\x1EC7\x0074"
		"TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g"
	]
	where punyTest str outp = EncodingTest punycode str (map (fromIntegral.ord) outp)
