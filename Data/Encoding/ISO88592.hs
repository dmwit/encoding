{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88592 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88592" "8859-2.TXT" )