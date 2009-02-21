{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.CP1251 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1251" "CP1251.TXT" )