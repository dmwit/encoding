{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.CP1252 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1252" "CP1252.TXT" )