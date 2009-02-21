{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO885916 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO885916" "8859-16.TXT" )