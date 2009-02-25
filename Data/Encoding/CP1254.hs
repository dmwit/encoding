{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.CP1254 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1254" "CP1254.TXT" )