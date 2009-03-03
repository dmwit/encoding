{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.JISX0212 where

import Data.Encoding.Helper.Template (makeJISInstance)

$( makeJISInstance "JISX0212" "JIS0212.TXT" )