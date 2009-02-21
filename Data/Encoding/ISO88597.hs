{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO88597 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88597" "8859-7.TXT" )