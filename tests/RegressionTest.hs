{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString, pack)
import Data.Encoding
import Data.Word
import System.Random

-- for example:
import Data.Encoding.ISO2022JP
--main = generate ISO2022JP
main = test ISO2022JP
-- BEWARE! These things are _very_ memory-hungry if you don't compile with optimizations.
-- end example

randomRepeat f max = randomRIO (0, max) >>= flip replicateM f
randomString = randomRepeat randomIO
randomGoodString = randomRepeat . randomGoodChar
randomGoodChar f = let
	good = filter f [minBound..maxBound]
	n    = length good
	in do
	i <- randomRIO (0, n-1)
	return (good !! i)

generate enc = do
	let filename = show enc ++ ".regression"
	    randomGood = randomGoodString (encodeable enc)
	shortMixedEnc <- replicateM  300 (         randomString   10)
	shortGoodEnc  <- replicateM   30 (         randomGood     10)
	longMixedEnc  <- replicateM  300 (         randomString 1000)
	longGoodEnc   <- replicateM 3000 (         randomGood   1000)
	shortDec      <- replicateM  300 (pack <$> randomString   10)
	longDec       <- replicateM 3000 (pack <$> randomString 1000)
	writeFile filename (show
		[ (s, encodeStrictByteStringExplicit enc s)
		| ss <- [shortMixedEnc, shortGoodEnc, longMixedEnc, longGoodEnc]
		, s <- ss
		] ++ "\n")
	appendFile filename (show
		[ (bs, decodeStrictByteStringExplicit enc bs)
		| bss <- [shortDec, longDec]
		, bs <- bss
		] ++ "\n")

complain action input expected actual = when (expected /= actual) . putStrLn . concat $
	[ "when "
	, pad action
	, show input
	, "\n"
	, pad "expected"
	, show expected
	, "\n"
	, pad "but got"
	, show actual
	]
	where
	size  = maximum . map length $ [action, "expected", "but got"]
	pad s = s ++ replicate (size - length s) ' ' ++ ": "

test enc = do
	[encoded_, decoded_] <- lines <$> readFile (show enc ++ ".regression")
	let encoded = read encoded_
	    decoded = read decoded_
	forM_ encoded $ \(s , correctEncoding) -> complain "encoding"  s correctEncoding (encodeStrictByteStringExplicit enc  s)
	forM_ decoded $ \(bs, correctDecoding) -> complain "decoding" bs correctDecoding (decodeStrictByteStringExplicit enc bs)
