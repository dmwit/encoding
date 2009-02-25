{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88599 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88599" "8859-9.TXT" )