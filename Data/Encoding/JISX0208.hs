{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.JISX0208 where

import Data.Encoding.Helper.Template (makeJISInstance)

$( makeJISInstance "JISX0208" "JIS0208.TXT" )