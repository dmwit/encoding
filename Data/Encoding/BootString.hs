{-# LANGUAGE DeriveDataTypeable #-}
{- | This implements BootString en- and decoding, the foundation of Punycode
 -}
module Data.Encoding.BootString
	(BootString(..)
	,punycode) where

import Data.Encoding.Base
import Data.Encoding.Exception
import Data.Encoding.ByteSource
import Data.Encoding.ByteSink
import Control.Throws
import Data.Word
import Data.List (unfoldr,partition,find)
import Data.Char (ord,chr)
import Data.Typeable
import Control.Monad (when)

data BootString = BootString
	{base :: Int
	,tmin :: Int
	,tmax :: Int
	,skew :: Int
	,damp :: Int
	,init_bias :: Int
	,init_n    :: Int
	}
	deriving (Show,Eq,Typeable)

punycode :: BootString
punycode = BootString
	{base = 36
	,tmin = 1
	,tmax = 26
	,skew = 38
	,damp = 700
	,init_bias = 72
	,init_n    = 0x80
	}

punyValue :: ByteSource m => Word8 -> m Int
punyValue c
	| n <  0x30 = norep
	| n <= 0x39 = return $ n-0x30+26
	| n <  0x41 = norep
	| n <= 0x5A = return $ n-0x41
	| n <  0x61 = norep
	| n <= 0x7A = return $ n-0x61
	| otherwise = norep
	where
	n = fromIntegral c
	norep = throwException (IllegalCharacter c)

punyChar :: ByteSink m => Int -> m Word8
punyChar c
	| c < 0  = norep
	| c < 26 = return $ fromIntegral $ 0x61+c
	| c < 36 = return $ fromIntegral $ 0x30+c-26
	| otherwise = norep
	where
	norep = throwException (HasNoRepresentation (chr c))

getT :: BootString -> Int -> Int -> Int
getT bs k bias
	| k <= bias + (tmin bs) = tmin bs
	| k >= bias + (tmax bs) = tmax bs
	| otherwise             = k-bias

adapt :: BootString -> Int -> Int -> Bool -> Int
adapt bs delta numpoints firsttime = let
	delta1 = if firsttime
		then delta `div` (damp bs)
		else delta `div` 2
	delta2 = delta1 + (delta1 `div` numpoints)
	(rd,rk) = head
		$ filter ((<=((base bs - tmin bs) * (tmax bs)) `div` 2).fst)
		$ iterate (\(d,k) -> (d `div` (base bs - tmin bs),k+(base bs))) (delta2,0)
	in rk + (((base bs - tmin bs +1) * rd) `div` (rd + skew bs))

decodeValue :: ByteSource m => BootString -> Int -> Int -> Int -> Int -> [Int] -> m (Int,[Int])
decodeValue bs bias i k w (x:xs)
	| x >= base bs                     = throwException OutOfRange
	| x > (maxBound - i) `div` w       = throwException OutOfRange
	| x <  t                           = return (ni,xs)
	| w > maxBound `div` (base bs - t) = throwException OutOfRange
        | null xs                          = throwException OutOfRange
	| otherwise = decodeValue bs bias ni (k+base bs) (w*(base bs - t)) xs
	where
	ni = i + x*w
	t  = getT bs k bias

decodeValues :: ByteSource m => BootString -> Int -> [Int] -> m [(Char,Int)]
decodeValues bs len xs = decodeValues' bs (init_n bs) 0 (init_bias bs) len xs

decodeValues' :: ByteSource m => BootString -> Int -> Int -> Int -> Int -> [Int] -> m [(Char,Int)]
decodeValues' bs n i bias len [] = return []
decodeValues' bs n i bias len xs = do
  (ni,rst) <- decodeValue bs bias i (base bs) 1 xs
  let (dn,nni) = ni `divMod` (len+1)
  let nn = n+dn
  if dn > maxBound - n
     then throwException OutOfRange
     else (do
            rest <- decodeValues' bs nn (nni+1) (adapt bs (ni-i) (len+1) (i==0)) (len+1) rst
            return $ (chr $ nn,nni):rest
          )
                                   
insertDeltas :: [(a,Int)] -> [a] -> [a]
insertDeltas [] str     = str
insertDeltas ((c,p):xs) str = let
	(l,r) = splitAt p str
	in insertDeltas xs (l++[c]++r)

punyDecode :: ByteSource m => [Word8] -> [Word8] -> m String
punyDecode base ext = do
  pvals <- mapM punyValue ext
  vals <- decodeValues punycode (length base) pvals
  return $ insertDeltas vals (map (chr.fromIntegral) base)
  
encodeValue :: BootString -> Int -> Int -> Int -> Int -> [Int]
encodeValue bs bias delta n c = unfoldr (\(q,k,out) -> let
		t = getT bs k bias
		(nq,dc) = (q-t) `divMod` (base bs - t)
		in if out
			then Nothing
			else (if q < t
				then Just (q,(q,k+base bs,True))
				else Just (t + dc,(nq,k+base bs,False)))
		) (delta,base bs,False)

encodeValues' :: BootString -> Int -> Int -> Int -> Int -> Int -> [Int] -> (Int,Int,Int,[Int])
encodeValues' _  _ h bias delta _ []     = (delta,h,bias,[])
encodeValues' bs b h bias delta n (c:cs) = case compare c n of
	LT -> encodeValues' bs b h bias (delta+1) n cs
	GT -> encodeValues' bs b h bias delta n cs
	EQ -> let
		(ndelta,nh,nbias,rest) = encodeValues' bs b (h+1) (adapt bs delta (h+1) (h==b)) 0 n cs
		xs = encodeValue bs bias delta n c
		in (ndelta,nh,nbias,xs++rest)

encodeValues :: BootString -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int]
encodeValues bs b l h bias delta n cps
	| h == l = []
	| otherwise = outp++encodeValues bs b l nh nbias (ndelta+1) (m+1) cps
	where
	m = minimum (filter (>=n) cps)
	(ndelta,nh,nbias,outp) = encodeValues' bs b h bias (delta + (m - n)*(h + 1)) m cps

breakLast :: (a -> Bool) -> [a] -> Maybe ([a],[a])
breakLast p xs = do
  (bf,af,ind) <- breakLast' 0 Nothing p xs
  return (bf,af)
    where
      breakLast' n r p [] = do
        v <- r
        return ([],[],v)
      breakLast' n r p (x:xs) = let res = if p x
                                          then breakLast' (n+1) (Just n) p xs
                                          else breakLast' (n+1) r p xs
                              in do
                                (bf,af,v) <- res
                                return $ if n<v then (x:bf,af,v) else (bf,x:af,v)
                          

instance Encoding BootString where
    encodeChar _ c = error "Data.Encoding.BootString.encodeChar: Please use 'encode' for encoding BootStrings"
    decodeChar _ = error "Data.Encoding.BootString.decodeChar: Please use 'decode' for decoding BootStrings"
    encode bs str = let (base,nbase) = partition (\c -> ord c < init_n bs) str
	                b = length base
                    in do
                      res <- mapM punyChar $ encodeValues bs b (length str) b (init_bias bs) 0 (init_n bs) (map ord str)
                      when (not $ null base) $ do
                               mapM_ (pushWord8.fromIntegral.ord) base
                               pushWord8 (fromIntegral $ ord '-')
                      mapM_ pushWord8 res
    decode bs = do
      wrds <- untilM sourceEmpty fetchWord8
      let m = fromIntegral $ ord '-'
      case breakLast (==m) wrds of
        Just ([],_) -> throwException (IllegalCharacter m)
	Just (base,_:nbase) -> case find (\w -> fromIntegral w > init_n bs) base of
                                Nothing -> punyDecode base nbase
                                Just ww -> throwException (IllegalCharacter ww)
	Nothing -> punyDecode [] wrds
    encodeable bs c = True -- XXX: hm, really?