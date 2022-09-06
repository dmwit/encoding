{-# LANGUAGE ParallelListComp, CPP #-}
module Data.Encoding.Preprocessor.XMLMappingBuilder where

import Data.Word
import Data.List
import Data.Ord
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array.Static.Builder
import Data.CharMap.Builder
import Data.Encoding.Preprocessor.XMLMapping
import Distribution.Simple.PreProcess
import System.FilePath
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.XmlContent
import Control.Exception (assert)

xmlPreprocessor :: PreProcessor
xmlPreprocessor = PreProcessor
                  { platformIndependent = True
#if MIN_VERSION_Cabal(3,8,0)
                  , ppOrdering=unsorted
#endif
                  , runPreProcessor = \src trg verb -> do
                                        createModuleFromFile src trg
                  }

description :: CharacterMapping -> Maybe String
description (CharacterMapping attrs _ _ _) = characterMappingDescription attrs

createModuleFromFile (sbase,sfile) (tbase,tfile) = do
  xml <- testFile (sbase </> sfile)
  let (dir,fn) = splitFileName sfile
  let (bname,ext) = splitExtensions fn
  let dirs = splitDirectories dir
  let body = buildDecisionTree minBound maxBound "ch" (encodingElements xml)
  let body2 = createDecoding (states xml) (decodingElements xml)
  let mpname = "encoding_map_"++bname
  let mp = buildCharMap $ [SingleMapping c w | (c,w) <- assignments xml]
           ++[ RangeMapping
               st end
               (foldl (\v (w,mi,ma) -> v*((fromIntegral $ ma-mi)+1) + (fromIntegral (w-mi))) 0 (zip3 bfirst bmin bmax))
               [(min,max-min+1) | min <- bmin | max <- bmax]
               | (st,end,bfirst,blast,bmin,bmax) <- ranges xml ]
  writeFile (tbase</>tfile) $ unlines $
             ["{- This file has been auto-generated. Do not edit it. -}"
             ,"{-# LANGUAGE MagicHash,DeriveDataTypeable #-}"]++
                (case description xml of
                   Nothing -> []
                   Just str -> ["{- | "++str++" -}"]) ++
                ["module "++concat (intersperse "." (dirs++[bname]))
                ,"  ("++bname++"("++bname++"))"
                ,"  where"
                ,""
                ,"import Control.Throws"
                ,"import Data.Encoding.Base"
                ,"import Data.Encoding.ByteSink"
                ,"import Data.Encoding.ByteSource"
                ,"import Data.Encoding.Exception"
                ,"import Data.Array.Static"
                ,"import Data.Map.Static"
                ,"import Data.CharMap"
                ,"import Data.Char"
                ,"import Data.Word"
                ,"import Data.Typeable"
                ,""
                ,"data "++bname++" = "++bname
                ,"  deriving (Eq,Show,Typeable)"
                ,""
                ,mpname++" :: CharMap"
                ,mpname++" = "++mp
                ,""
                ,"instance Encoding "++bname++" where"
                ,"  encodeChar _ ch = mapEncode ch "++mpname
                ,"  decodeChar _ = "++body2
                ,"  encodeable _ ch = mapMember ch "++mpname
                ]

decodingValueRange :: [(Word8,Word8)] -> DecodingElement -> (Int,Int)
decodingValueRange path (DecodingElement c ws)
    = let v = foldl (\n (w,(lo,up)) -> n*((fromIntegral $ up-lo)+1) + (fromIntegral $ w - lo)) 0 (zip ws path)
      in (v,v)
decodingValueRange path (DecodingRange first last bfirst blast bmin bmax)
    = assert (zip bmin bmax == path) $
      (decodingValue path bfirst
      ,decodingValue path blast)

decodingValue :: [(Word8,Word8)] -> [Word8] -> Int
decodingValue path ws
    = foldl (\n (w,(lo,up)) -> n*((fromIntegral $ up-lo) + 1) + (fromIntegral $ w - lo))
      0 (zip ws path)

type StateMachine = Map String [(Word8,Word8,String)]

createDecoding :: StateMachine -> [DecodingElement] -> String
createDecoding sm els = create' els [] 0 "FIRST"
    where
      create' els path n st = let trans = sortBy (\(s1,e1,st1) (s2,e2,st2) -> compare s1 s2) $ sm Map.! st
                              in "(fetchWord8 >>= \\w"++show n++" -> " ++ tree' n path els trans 0 255++")"

      tree' :: Int -> [(Word8,Word8)] -> [DecodingElement] -> [(Word8,Word8,String)] -> Word8 -> Word8 -> String
      tree' n path els [] _ _ = illWord $ "w"++show n
      tree' n path els [(s,e,nst)] bl br
          = let e1 = if s > bl
                     then "(if w"++show n++" < "++show s++" then "++illWord ("w"++show n)++" else "++e2++")"
                     else e2
                e2 = if e < br
                     then "(if w"++show n++" > "++show e++" then "++illWord ("w"++show n)++" else "++e3++")"
                     else e3
                e3 = if nst == "VALID"
                     then array' rpath sels
                     else "{- for "++nst++"-}" ++ create' nels npath (n+1) nst
                npath = (s,e):path
                rpath = reverse npath
                sels = sortBy (comparing (decodingValueRange rpath)) nels
                nels = filter (\el -> let (ll,lr) = (decodingLimits el)!!n in ll>=s && lr <= e) els
            in e1
      tree' n path els trans bl br
          = let (left,right@((b,_,_):_)) = splitAt (length trans `div` 2) trans
                (eleft,eright) = partition (\el -> fst ((decodingLimits el)!!n) < b) els
            in "(if w"++show n++" < "++show b++" then "++tree' n path eleft left bl (b-1)
                   ++" else "++tree' n path eright right b br++")"

      array' path els = let grps = groupBy (\e1 e2 -> case e1 of
                                                       DecodingRange _ _ _ _ _ _ -> False
                                                       _ -> case e2 of
                                                             DecodingRange _ _ _ _ _ _ -> False
                                                             _ -> True
                                           ) els
                            ranges = map (\(l,u) -> (fromIntegral $ u-l)+1) path
                            val = foldl (\expr (r,n,m) -> "("++expr++"*"++show r++"+(fromIntegral w"++show n++"-"++show m++"))")
                                  "0"
                                  (zip3 ranges [0..] (map fst path))
                            offset = (product ranges)-1
                        in "(let val = " ++ val ++ " in "++array'' path grps 0 offset++")"

      array'' path [] _ _ = "throwException (IllegalRepresentation ["++concat (intersperse "," (zipWith (\n _ -> "w"++show n) [0..] path))++"])"
      array'' path [grp] lo up
          = case grp of
              [DecodingRange first end bfirst bend bmin bmax] ->
                  let ranges = map (\(l,u) -> (fromIntegral $ u-l)+1) path
                      off = foldl (\v (r,c,m) -> v*r+(fromIntegral $ c-m)) 0 (zip3 ranges bfirst bmin)
                      equalranges = and $ zipWith (==) path (zip bmin bmax)
                  in if equalranges
                     then "(return (chr (val + ("++show (ord first - off)++"))))"
                     else error "Can't have a range that has a different range..."
              _ -> let chars = fillRange lo $ map (\el@(DecodingElement c _) -> (c,fst $ decodingValueRange path el)) grp
                  in "(return (("++buildStaticArray (lo,up) chars++")!val))"
      array'' path grps lo up = let (left,right@(brk:_)) = splitAt (length grps `div` 2) grps
                                    (off,_) = decodingValueRange path (head brk)
                                in "(if val < "++show off++" then "++array'' path left lo (off-1)
                                       ++" else "++array'' path right off up++")"

fillRange :: Int -> [(Char,Int)] -> [Char]
fillRange s [] = []
fillRange s all@((c,i):cs) = case compare i s of
                               GT -> '\0':fillRange (s+1) all
                               LT -> error $ "Char out of range "++show (take 10 all)
                               EQ -> c:fillRange (s+1) cs

states :: CharacterMapping -> StateMachine
states (CharacterMapping attrs hist val ass)
    = case val of
        OneOf2 (Validity (NonEmpty lst)) -> Map.fromListWith (++) $
                                           map (\st -> let BS [start] = stateS st
                                                           end = case stateE st of
                                                                   Nothing -> start
                                                                   Just (BS [rend]) -> rend
                                                      in (stateType st,[(start,end,stateNext st)])) lst
        _ -> error "Mapping doesn't contain validity section"

decodingElements :: CharacterMapping -> [DecodingElement]
decodingElements mp = map (\(c,ws) -> DecodingElement c ws) (assignments mp)
                      ++ map (\(fi,la,bfi,bla,bmi,bma) -> DecodingRange fi la bfi bla bmi bma) (ranges mp)

illWord :: String -> String
illWord n = "throwException (IllegalCharacter "++n++")"

decodingLimits :: DecodingElement -> [(Word8,Word8)]
decodingLimits (DecodingElement _ ws) = map (\w -> (w,w)) ws
decodingLimits (DecodingRange _ _ bfirst blast bmin bmax) = lim' False (zip4 bfirst blast bmin bmax)
    where
      lim' dec [] = []
      lim' dec ((fi,la,mi,ma):xs) = if dec
                                    then (mi,ma):(lim' dec xs)
                                    else (fi,la):(lim' (fi/=la) xs)

decodingLength :: DecodingElement -> Int
decodingLength (DecodingRange _ _ first _ _ _) = length first
decodingLength (DecodingElement _ ws) = length ws

decodingElementCount :: DecodingElement -> Int
decodingElementCount (DecodingRange s e _ _ _ _) = ord e - ord s
decodingElementCount (DecodingElement _ _) = 1

data DecodingElement
    = DecodingRange Char Char [Word8] [Word8] [Word8] [Word8]
    | DecodingElement Char [Word8]
    deriving Show

norep :: String -> String
norep var = "(throwException $ HasNoRepresentation "++var++")"

buildDecisionTree :: Char -> Char -> String -> [EncodingElement] -> String
buildDecisionTree l r var [] = norep var
buildDecisionTree l r var [el]
    = let e1 = if l < startChar el
               then "(if "++var++" < "++show (startChar el)++" then "++norep var++" else "++e2++")"
               else e2
          e2 = if r > endChar el
               then "(if "++var++" > "++show (endChar el)++" then "++norep var++" else "++e3++")"
               else e3
          e3 = buildEncoding el var
      in e1
buildDecisionTree ll lr var els
    = let (l,r@(sep:_)) = splitAt (length els `div` 2) els
      in "(if "++var++" < "++show (startChar sep)
             ++" then ("++(buildDecisionTree ll (pred $ startChar sep) var l)++")"
             ++" else ("++(buildDecisionTree (endChar sep) lr var r)++")"
             ++")"

buildEncoding :: EncodingElement -> String -> String
buildEncoding (EncodingRange start end bf bl bmin bmax) var
    = let ranges :: [Int]
          ranges = map fromIntegral $ zipWith (-) bmax bmin
      in  "(let num = (ord "++var++") - ("++show (ord start - (foldl (\n (r,vf,vm) -> n*(r+1) + (fromIntegral (vf-vm))) 0 (zip3 ranges bf bmin)))++")"
              ++concat ([ " ; (p"++show n++",r"++show n++") = "
                          ++(if n==1 then "num" else "p"++show (n-1))
                          ++" `divMod` "++show (r+1)
                          | r <- reverse ranges | n <- [1..] ])
              ++" in "
              ++concat (intersperse " >> " (reverse ["pushWord8 (fromIntegral (r"++show n++" + "++show w++"))" | n <- [1..] | w <- reverse bmin]))
              ++")"
buildEncoding (EncodingGroup start end encs) var
    = let findParams st []     = st
          findParams st (x:xs) = findParams (case compare (length x) (fst st) of
                                               LT -> (fst st,False)
                                               GT -> (length x,False)
                                               EQ -> st) xs
          (mx,same) = findParams (length $ head encs,True) (tail encs)
      in if same
         then ("(let off = "++show mx++"*(ord "++var++" - "++show (ord start)++") ; arr = "
               ++buildStaticArray (0,(length encs)*mx-1) (concat encs)
               ++" in "
               ++concat (intersperse " >> " ["pushWord8 (arr!(off+"++show (n-1)++"))" | n <- [1..mx]])
               ++")")
         else ("(let off = "++show (mx+1)++"*((ord "++var++") - "++show (ord start)++") ; arr = "
               ++buildStaticArray (0,(length encs)*(mx+1)-1)
                                      (concat [(fromIntegral $ length e)
                                               :(e++replicate (mx-length e) 0) | e <- encs])
               ++ "::StaticArray Int Word8"
               ++" ; len = fromIntegral (arr!off)::Int ; bytes = map (\\n -> arr!(off+n)) [1..len]"
               ++" in mapM_ pushWord8 bytes)") 

data EncodingElement
    = EncodingRange Char Char [Word8] [Word8] [Word8] [Word8]
    | EncodingGroup Char Char [[Word8]]
    deriving Show

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f [] ys = ys
mergeBy f xs [] = xs
mergeBy f (x:xs) (y:ys)
    = case f x y of
        LT -> x:mergeBy f xs (y:ys)
        _  -> y:mergeBy f (x:xs) ys

startChar :: EncodingElement -> Char
startChar (EncodingRange c _ _ _ _ _) = c
startChar (EncodingGroup c _ _) = c

endChar :: EncodingElement -> Char
endChar (EncodingRange _ c _ _ _ _) = c
endChar (EncodingGroup _ c _) = c


encodingElements :: CharacterMapping -> [EncodingElement]
encodingElements mp = mergeBy (comparing startChar)
                      (buildGroups $ sortAssignments $ assignments mp)
                      (encodingRanges $ ranges mp)

assignments :: CharacterMapping -> [(Char,[Word8])]
assignments (CharacterMapping _ _ _ (Assignments _ ass _ _ _ ranges))
    = map (\a -> let CP [cp] = aU a
                     BS bs = aB a
                in (cp,bs)
          ) ass

encodingRanges :: [(Char,Char,[Word8],[Word8],[Word8],[Word8])] -> [EncodingElement]
encodingRanges lst = sortBy (comparing (\(EncodingRange c _ _ _ _ _) -> c)) $
                     map (\(ufirst,ulast,bfirst,blast,bmin,bmax) -> EncodingRange ufirst ulast bfirst blast bmin bmax) lst

ranges :: CharacterMapping -> [(Char,Char,[Word8],[Word8],[Word8],[Word8])]
ranges (CharacterMapping _ _ _ (Assignments _ ass _ _ _ ranges))
    = map (\r -> let BS bfirst   = rangeBFirst r
                     BS blast    = rangeBLast r
                     CP [ufirst] = rangeUFirst r
                     CP [ulast]  = rangeULast r
                     BS bmin     = rangeBMin r
                     BS bmax     = rangeBMax r
                in (ufirst,ulast,bfirst,blast,bmin,bmax)
          ) ranges


sortAssignments :: [(Char,[Word8])] -> [(Char,[Word8])]
sortAssignments = sortBy (comparing fst)

buildGroups :: [(Char,[Word8])] -> [EncodingElement]
buildGroups [] = []
buildGroups ((c,bs):rest) = (EncodingGroup c end (bs:wrds)):buildGroups oth
    where
      (end,wrds,oth) = group c rest

      group n [] = (n,[],[])
      group n all@((c,bs):rest)
          | succ n == c = let (e,res,oth) = group c rest
                          in (e,bs:res,oth)
          | otherwise = (n,[],all)
