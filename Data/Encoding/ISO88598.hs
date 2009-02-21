{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO88598 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88598" "8859-8.TXT" )