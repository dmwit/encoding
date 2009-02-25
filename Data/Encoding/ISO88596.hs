{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88596 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88596" "8859-6.TXT" )