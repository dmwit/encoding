{-# LANGUAGE TemplateHaskell #-}
module Data.Encoding.ISO885910 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO885910" "8859-10.TXT" )