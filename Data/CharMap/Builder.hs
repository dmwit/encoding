module Data.CharMap.Builder where

import Data.Map.Static.Builder

import Data.List
import Data.Ord
import Data.Char
import Data.Bits
import Data.Word

data BuildingBlock
    = SingleMapping Char [Word8]
    | RangeMapping Char Char Int [(Word8,Word8)]

charRange :: BuildingBlock -> (Char,Char)
charRange (SingleMapping c _) = (c,c)
charRange (RangeMapping s e _ _) = (s,e)

mappingLength :: BuildingBlock -> Int
mappingLength (SingleMapping _ w) = length w
mappingLength (RangeMapping _ _ _ ws) = length ws

isRange :: BuildingBlock -> Bool
isRange (SingleMapping _ _) = False
isRange (RangeMapping _ _ _ _) = True

buildCharMap :: [BuildingBlock] -> String
buildCharMap lst = let slst = sortBy (comparing (fst.charRange)) lst
                       grps = groupBy (\x y -> (not (isRange x || isRange y))
                                              && mappingLength x == mappingLength y
                                      ) slst

                       split' xs = splitAt (length xs `div` 2) xs
                       
                       build' [] _ _ = "DeadEnd"
                       build' [[RangeMapping st end off (x:xs)]] bl br
                           = let e1 = if bl < st
                                      then "Node ("++show st++") DeadEnd ("++e2++")"
                                      else e2
                                 e2 = if br>end
                                      then "Node ("++show end++") ("++e3++") DeadEnd"
                                      else e3
                                 e3 = "LeafRange"++show (length xs+1)++" ("++show (ord st - off)++") "
                                      ++show (fst x)++concat (map (\(w,r) -> " "++show w++" "++show r) xs)
                             in e1
                       build' [mps@((SingleMapping _ w):_)] bl br
                           = "LeafMap"++show (length w)++" ("
                             ++(case length w of
                                  1 -> buildStaticMap (map (\(SingleMapping c [w]) -> (c,w)) mps)
                                  2 -> buildStaticMap $ map (\(SingleMapping c [w1,w2])
                                                               -> (c,((fromIntegral w1) `shiftL` 8) .|. (fromIntegral w2)::Word16)
                                                           ) mps
                                  4 -> buildStaticMap $ map (\(SingleMapping c [w1,w2,w3,w4])
                                                                -> (c,((fromIntegral w1) `shiftL` 24)
                                                                   .|. ((fromIntegral w2) `shiftL` 16)
                                                                   .|. ((fromIntegral w3) `shiftL` 8)
                                                                   .|. (fromIntegral w4)::Word32)
                                                           ) mps)++")"
                       build' mps bl br = let (l,r@((spl:_):_)) = split' mps
                                              (el,_) = charRange spl
                                          in "Node ("++show el++") ("++build' l bl (pred el)++") ("++
                                             build' r el br++")"
                                               
                   in build' grps minBound maxBound