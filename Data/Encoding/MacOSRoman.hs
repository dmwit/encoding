{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.MacOSRoman where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "MacOSRoman" "ROMAN.TXT" )