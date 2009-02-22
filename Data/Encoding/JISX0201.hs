{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.JISX0201 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "JISX0201" "JIS0201.TXT" )