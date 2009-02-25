{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.CP1258 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1258" "CP1258.TXT" )