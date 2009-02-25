{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO885914 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO885914" "8859-14.TXT" )