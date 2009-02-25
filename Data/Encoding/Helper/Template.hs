{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.Helper.Template where

import Data.Encoding.Base
import Data.Bits
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Map as Map (fromList,lookup)
import Data.Array.Unboxed
import Data.Typeable
import Language.Haskell.TH

makeISOInstance :: String -> FilePath -> Q [Dec]
makeISOInstance name file = do
  trans <- runIO (readTranslation file)
  mp <- encodingMap (validTranslations trans)
  arr <- decodingArray (fillTranslations 0 255 trans)
  return $ encodingInstance 'encodeWithMap 'decodeWithArray 'encodeableWithMap name mp arr

makeJISInstance :: String -> FilePath -> Q [Dec]
makeJISInstance name file = do
  trans <- runIO (readJISTranslation file)
  mp <- encodingMap2 (validTranslations trans)
  arr <- decodingArray2 (fillTranslations (0x21,0x21) (0x7E,0x7E) trans)
  return $ encodingInstance 'encodeWithMap2 'decodeWithArray2 'encodeableWithMap name mp arr

encodingInstance :: Name -> Name -> Name -> String -> Exp -> Exp -> [Dec]
encodingInstance enc dec able name mp arr
    = [ DataD [] rname [] [NormalC rname []] [''Show,''Eq,''Typeable]
      , ValD (VarP rmp) (NormalB mp) []
      , InstanceD [] (AppT (ConT ''Encoding) (ConT rname))
                      [FunD 'encodeChar
                       [Clause [WildP] (NormalB $ AppE (VarE enc) (VarE rmp))
                        []
                       ]
                      ,FunD 'decodeChar
                       [Clause [WildP] (NormalB $ AppE (VarE dec) (VarE rarr))
                        [ValD (VarP rarr) (NormalB arr) []]
                       ]
                      ,FunD 'encodeable
                       [Clause [WildP] (NormalB $ AppE (VarE able) (VarE rmp))
                        []
                       ]
                      ]
      ]
    where
      rname = mkName name
      rarr = mkName "arr"
      rmp = mkName ("decoding_map_"++name)

createCharArray :: [(Integer,Maybe Char)] -> Integer -> Integer -> Q Exp
createCharArray lst f t = createArray (map (\(x,y) ->
                                                (LitE $ IntegerL x,mbCharToExp y)
                                           ) lst) (LitE $ IntegerL f) (LitE $ IntegerL t)

createCharArray2 :: [((Integer,Integer),Maybe Char)] -> (Integer,Integer) -> (Integer,Integer) -> Q Exp
createCharArray2 lst (f1,f2) (t1,t2)
    = createArray (map (\((x1,x2),y) ->
                            (TupE [integerExp x1,integerExp x2],mbCharToExp y)
                       ) lst)
      (TupE [integerExp f1,integerExp f2])
      (TupE [integerExp t1,integerExp t2])

integerExp :: Integer -> Exp
integerExp i = LitE $ IntegerL i

mbCharToExp :: Maybe Char -> Exp
mbCharToExp Nothing = LitE (IntegerL (-1))
mbCharToExp (Just c) = LitE (IntegerL $ fromIntegral $ ord c)

createArray :: [(Exp,Exp)] -> Exp -> Exp -> Q Exp
createArray lst from to
    = return $ AppE
      (AppE
       (VarE 'array)
       (TupE [from,to]))
      (ListE [TupE [x,y] | (x,y) <- lst])

decodingArray :: [(Integer,Maybe Char)] -> Q Exp
decodingArray trans = createCharArray trans 0 255

decodingArray2 :: [((Integer,Integer),Maybe Char)] -> Q Exp
decodingArray2 trans = createCharArray2 trans (0x21,0x21) (0x7E,0x7E)

encodingMap :: [(Integer,Char)] -> Q Exp
encodingMap trans = return $ AppE
                    (VarE 'fromList)
		    (ListE [ TupE [LitE $ CharL to,LitE $ IntegerL from]
			     | (from,to) <- trans])

encodingMap2 :: [((Integer,Integer),Char)] -> Q Exp
encodingMap2 trans = return $ AppE
                     (VarE 'fromList)
                     (ListE [ TupE [LitE $ CharL to,TupE [integerExp f1,integerExp f2]]
                              | ((f1,f2),to) <- trans])

readTranslation :: FilePath -> IO [(Integer,Maybe Char)]
readTranslation file = do
  cont <- readFile file
  return $ mapMaybe (\ln -> case ln of
                             [src] -> Just (src,Nothing)
                             [src,trg] -> Just (src,Just $ chr $ fromIntegral trg)
                             _ ->  Nothing) (parseTranslationTable cont)

readJISTranslation :: FilePath -> IO [((Integer,Integer),Maybe Char)]
readJISTranslation file = do
  cont <- readFile file
  return $ mapMaybe (\ln -> case ln of
                             [_,src] -> Just ((src `shiftR` 8,src .&. 0xFF),Nothing)
                             [_,src,trg] -> Just ((src `shiftR` 8,src .&. 0xFF),Just $ chr $ fromIntegral trg)
                             _ -> Nothing) (parseTranslationTable cont)

parseTranslationTable :: String -> [[Integer]]
parseTranslationTable cont = filter (not.null) (map (\ln -> map read (takeWhile ((/='#').head) (words ln))) (lines cont))

fillTranslations :: Ix a => a -> a -> [(a,Maybe Char)] -> [(a,Maybe Char)]
fillTranslations f t = merge (range (f,t))
    where
      merge xs [] = map (\x -> (x,Nothing)) xs
      merge [] _  = error "Data.Encoding.Helper.Template.fillTranslations: Character translations out of range"
      merge (x:xs) (y:ys) = if x < fst y
                            then (x,Nothing):(merge xs (y:ys))
                            else y:(merge xs ys)

validTranslations :: [(a,Maybe Char)] -> [(a,Char)]
validTranslations = mapMaybe (\(n,mc) -> case mc of
                                          Nothing -> Nothing
                                          Just c -> Just (n,c))