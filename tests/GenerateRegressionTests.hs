{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Data.ByteString (pack)
import Data.Encoding
import System.Random

-- for example:
import Data.Encoding.ISO2022JP
main = generate ISO2022JP
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
	shortMixedEnc <- replicateM  300 (randomString   10)
	shortGoodEnc  <- replicateM   30 (randomGood     10)
	longMixedEnc  <- replicateM  300 (randomString 1000)
	longGoodEnc   <- replicateM 3000 (randomGood   1000)
	shortDec      <- replicateM  300 (randomString   10)
	longDec       <- replicateM 3000 (randomString 1000)
	writeFile filename (show
		[ (s, encodeStrictByteStringExplicit enc s)
		| ss <- [shortMixedEnc, shortGoodEnc, longMixedEnc, longGoodEnc]
		, s <- ss
		] ++ "\n")
	appendFile filename (show
		[ (bs, decodeStrictByteStringExplicit enc (pack bs))
		| bss <- [shortDec, longDec]
		, bs <- bss
		] ++ "\n")
