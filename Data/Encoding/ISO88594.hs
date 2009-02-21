{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO88594 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88594" "8859-4.TXT" )