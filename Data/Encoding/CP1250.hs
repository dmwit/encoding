{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.CP1250 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1250" "CP1250.TXT" )