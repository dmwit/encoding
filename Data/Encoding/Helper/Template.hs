{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.Helper.Template where

import Data.Encoding.Base
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Map as Map (fromList,lookup)
import Data.Array
import Language.Haskell.TH

makeISOInstance :: String -> FilePath -> Q [Dec]
makeISOInstance name file = do
  let rname = mkName name
  trans <- runIO (readTranslation file)
  mp <- encodingMap (validTranslations trans)
  arr <- decodingArray (fillTranslations trans)
  return [ DataD [] rname [] [NormalC rname []] [''Show]
         , InstanceD [] (AppT (ConT ''Encoding) (ConT rname))
                     [FunD 'encodeChar
                               [Clause [WildP] (NormalB $ AppE (VarE 'encodeWithMap) (VarE $ mkName "mp"))
                                [ValD (VarP $ mkName "mp") (NormalB mp) []]
                               ]
                     ,FunD 'decodeChar
                               [Clause [WildP] (NormalB $ AppE (VarE 'decodeWithArray) (VarE $ mkName "arr"))
                                [ValD (VarP $ mkName "arr") (NormalB arr) []]
                               ]
                     ]
         ]
  
createCharArray :: [(Integer,Maybe Char)] -> Integer -> Integer -> Q Exp
createCharArray lst = createArray (map (\(x,y) -> (x,case y of
                                                      Nothing -> ConE 'Nothing
                                                      Just c -> AppE (ConE 'Just) (LitE $ CharL c))
                                       ) lst)


createArray :: [(Integer,Exp)] -> Integer -> Integer -> Q Exp
createArray lst from to = return $ AppE
	(AppE
		(VarE 'array)
		(TupE [LitE $ IntegerL from,LitE $ IntegerL to]))
	(ListE [ TupE [LitE $ IntegerL x,y]
		| (x,y) <- lst ])

decodingArray :: [(Integer,Maybe Char)] -> Q Exp
decodingArray trans = createCharArray trans 0 255

encodingMap :: [(Integer,Char)] -> Q Exp
encodingMap trans = return $ AppE
                    (VarE 'fromList)
		    (ListE [ TupE [LitE $ CharL to,LitE $ IntegerL from]
			     | (from,to) <- trans])

readTranslation :: FilePath -> IO [(Integer,Maybe Char)]
readTranslation file = do
	cont <- readFile file
	return $ mapMaybe (\ln -> case ln of
		[] -> Nothing
		('#':xs) -> Nothing
		_ -> case words ln of
			(src:"#UNDEFINED":_) -> Just (read src,Nothing) -- XXX: Find a better way to handle this
			(src:trg:_) -> Just (read src,Just $ chr $ read trg)
			_ -> Nothing
			) (lines cont)

fillTranslations :: [(Integer,Maybe Char)] -> [(Integer,Maybe Char)]
fillTranslations = fillTranslations' (-1)
    where
      fillTranslations' n ((n',c):cs) = (map (\i -> (i,Nothing)) [n+1..n'-1])++((n',c):fillTranslations' n' cs)
      fillTranslations' n [] = map (\i -> (i,Nothing)) [n+1..255]

validTranslations :: [(Integer,Maybe Char)] -> [(Integer,Char)]
validTranslations = mapMaybe (\(n,mc) -> case mc of
                                          Nothing -> Nothing
                                          Just c -> Just (n,c))