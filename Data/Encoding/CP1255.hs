{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.CP1255 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1255" "CP1255.TXT" )