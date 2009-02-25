{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.ISO88595 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "ISO88595" "8859-5.TXT" )