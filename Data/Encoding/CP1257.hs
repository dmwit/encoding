{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.CP1257 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1257" "CP1257.TXT" )