{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO885915 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO885915" "8859-15.TXT" )