{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88593 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88593" "8859-3.TXT" )