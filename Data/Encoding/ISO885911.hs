{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO885911 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO885911" "8859-11.TXT" )