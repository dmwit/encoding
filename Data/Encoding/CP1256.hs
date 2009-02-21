{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.CP1256 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1256" "CP1256.TXT" )