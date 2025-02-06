{-# LANGUAGE CPP #-}
module Data.Encoding.Preprocessor.Mapping where

import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.IO
import System.FilePath
import Data.List (intersperse,unfoldr)
import Data.Maybe
import Data.Char
import Data.Word
import Data.Ix
import Data.Bits
import Data.Array.Static.Builder
import Data.CharMap.Builder

data MappingType
    = ISOMapping
    | JISMapping
    deriving (Eq,Ord,Show,Read)

readTranslation :: Int -> FilePath -> IO ([(Integer,Maybe Char)],[String])
readTranslation offset file = do
  cont <- fmap parseTranslationTable $ readFile file
  let docstr = mapMaybe snd (takeWhile (null.fst) cont)
  let trans =  mapMaybe (\(ln,comm) -> case drop offset ln of
                                        [src] -> Just (src,Nothing)
                                        [src,trg] -> Just (src,Just $ chr $ fromIntegral trg)
                                        _ ->  Nothing) cont
  return (trans,docstr)

parseTranslationTable :: String -> [([Integer],Maybe String)]
parseTranslationTable cont = map (\ln -> let (trans,comm) = break (=='#') ln
                                         in (map read (words trans),case comm of
                                                                      "" -> Nothing
                                                                      _ -> Just (tail comm))
                                 ) (lines cont)

buildDocTable :: [(Integer,Maybe Char)] -> [String]
buildDocTable = intersperse "".
                map (\(i,mbc) -> show i ++ (case mbc of
                                             Nothing -> ""
                                             Just c -> "\t = &#"++show (ord c)++"; ("++show (ord c)++")"))

{-fillTranslations :: (Ix a,Show a) => a -> a -> [(a,Maybe Char)] -> [(a,Maybe Char)]
fillTranslations f t = merge (range (f,t))
    where
      merge xs [] = map (\x -> (x,Nothing)) xs
      merge [] cs  = error $ "Data.Encoding.Helper.Template.fillTranslations: Character translations out of range: " ++ show cs
      merge (x:xs) (y:ys) = if x < fst y
                            then (x,Nothing):(merge xs (y:ys))
                            else y:(merge xs ys)-}

fillTranslations :: (Enum a,Eq a) => [(a,Maybe Char)] -> (a,a,[Maybe Char])
fillTranslations [] = error "fillTranslations: zero elements"
fillTranslations ((s,c):rest) = let (e,r) = fill' s rest
                                    fill' cur [] = (cur,[])
                                    fill' cur all@((n,c):rest2) = if succ cur == n
                                                                  then (let (e',res) = fill' n rest2
                                                                        in (e',c:res))
                                                                  else (let (e',res) = fill' (succ cur) all
                                                                        in (e',Nothing:res))
                                in (s,e,c:r)

validTranslations :: [(a,Maybe Char)] -> [(a,Char)]
validTranslations = mapMaybe (\(n,mc) -> case mc of
                                          Nothing -> Nothing
                                          Just c -> Just (n,c))

mappingPreprocessor :: PreProcessor
mappingPreprocessor = PreProcessor
                  {platformIndependent = True
#if MIN_VERSION_Cabal(3,8,0)
                  ,ppOrdering=unsorted
#endif

                  ,runPreProcessor = \(sbase,sfile) (tbase,tfile) verb -> do
                                       let (dir,fn) = splitFileName sfile
                                       let (bname,ext) = splitExtensions fn
                                       let dirs = splitDirectories dir
                                       let tp = case ext of
                                                  ".mapping" -> ISOMapping
                                                  ".mapping2" -> JISMapping
                                       info verb (tfile++" generated from mapping "++sfile)
                                       preprocessMapping tp (sbase </> sfile) (tbase </> tfile) dirs bname
                  }

preprocessMapping :: MappingType -> FilePath -> FilePath -> [String] -> String -> IO ()
preprocessMapping tp src trg mods name = do
  (trans,doc) <- readTranslation 0 src
  let mod = concat $ intersperse "." (mods++[name])
  let wsize = case tp of
                ISOMapping -> 1
                JISMapping -> 2
  let bsize = show (wsize*8) ++ (if wsize > 1 then "be" else "")

  --let (larr,off,arr) = staticArray32 trans
  let (sarr,earr,els) = fillTranslations trans
  {-let (lmp,idx,val) = staticMap wsize trans-}
  let arrname = "decoding_array_"++name
  let mpname = "encoding_map_"++name
  let bcheck exp = if sarr/=0 || earr/=255
                   then ("(if "++
                         concat (intersperse "||" $ (if sarr/=0 then ["w<"++show sarr] else [])++(if earr/=255 then ["w>"++show earr] else []))++
                         " then throwException $ IllegalCharacter $ fromIntegral w else "++exp++")"
                        ) else exp
  let mp = buildCharMap (mapMaybe (\(i,c) -> do
                                     rc <- c
                                     return $ SingleMapping 
                                            rc 
                                            (reverse $ unfoldr (\(w,n) -> if n == 0 
                                                                         then Nothing 
                                                                         else Just (fromIntegral w,(w `shiftR` 8,n-1))) (i,wsize))
                             ) trans
                        )
  {-let mp = case wsize of
             1 -> buildStaticMap (mapMaybe (\(i,c) -> case c of
                                                      Nothing -> Nothing
                                                      Just rc -> Just (rc,fromIntegral i::Word8)) trans)
             2 -> buildStaticMap (mapMaybe (\(i,c) -> case c of
                                                      Nothing -> Nothing
                                                      Just rc -> Just (rc,fromIntegral i::Word16)) trans)-}
  writeFile trg $ unlines $
                ["{- This file has been auto-generated. Do not edit it. -}"
                ,"{-# LANGUAGE MagicHash,DeriveDataTypeable #-}"
                ]++(case doc of
                      [] -> ["{- |"]
                      _ -> ("{- | "++head doc):(map (\ln -> "     "++ln) (tail doc)))
                ++[""]
                ++buildDocTable trans
                ++[" -}"]
                ++
                ["module "++mod++"("++name++"(..)) where"
                ,""
                ,"import Data.Encoding.Base"
                ,"import Data.Encoding.ByteSource"
                ,"import Data.Encoding.ByteSink"
                ,"import Data.Encoding.Exception"
                ,"import Data.CharMap"
                ,"import Data.Array.Static"
                ,"import Data.Map.Static"
                ,"import Control.Throws"
                ,"import Prelude hiding (lookup)"
                ,"import Data.Word"
                ,""
                ,"import Data.Typeable"
                ,""
                ,"data "++name++" = "++name
                ,"  deriving (Show,Eq,Typeable)"
                ,""
                ,arrname++" = "++buildStaticArray (sarr,earr) els
                ,""
                ,mpname++" :: CharMap"
                ,mpname++" = "++mp
                ,""
                ,"instance Encoding "++name++" where"
                ,"  decodeChar _ = do"
                ,"    w <- fetchWord"++bsize
                ,"    "++bcheck "return ()"
                ,"    case "++arrname++"!w of"
                ,"      Nothing -> throwException $ IllegalCharacter $ fromIntegral w"
                ,"      Just c -> return c"
                ,"  encodeChar _ c = mapEncode c "++mpname
                ,"  encodeable _ c = mapMember c "++mpname
                ]
