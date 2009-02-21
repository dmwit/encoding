{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO885913 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO885913" "8859-13.TXT" )