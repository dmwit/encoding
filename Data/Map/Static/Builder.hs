module Data.Map.Static.Builder where

import Data.Static
import Data.Array.Static.Builder

import Data.List
import Data.Ord
import Data.Bits

buildStaticMap :: (StaticElement i,StaticElement e,Ord i) => [(i,e)] -> String
buildStaticMap lst = let  step :: Int -> [(i,e)] -> [(Int,(i,e))]
                          step n chunk = let ss = findSplitSize (length chunk)
                                             (h,d:t) = splitAt ss chunk
                                         in if null chunk
                                            then []
                                            else (n,d):((step (n*2) h)++(step (n*2+1) t))
                          checkHeap n [] = []
                          checkHeap n ((c,x):xs) = if c == n
                                                   then x:checkHeap (n+1) xs
                                                   else error $ "Heap is not consistent: Should be "++show n++" but is "++show c
                          uheap = sortBy (comparing fst) (step 1 slst)
                          slst = sortBy (comparing fst) lst
                          heap = checkHeap 1 $ sortBy (comparing fst) (step 1 slst)
                          len = length heap
                      in "StaticMap ("++buildStaticArray (1,len) (map fst heap)++") ("++buildStaticArray (1,len) (map snd heap)++")"

maxSize :: Int -> Int
maxSize d = (1 `shiftL` d) - 1

treeDepth :: Int -> Int
treeDepth sz = find' [0..]
    where
      find' (x:xs) = if 1 `shiftL` x > sz
                     then x
                     else find' xs

findSplitSize :: Int -> Int
findSplitSize len = let depth = treeDepth len
                        free = (maxSize depth) - len
                    in if free <= (1 `shiftL` (depth - 2))
                       then maxSize (depth - 1)
                       else len - (maxSize (depth - 2)) - 1
