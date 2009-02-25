{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Data.Encoding.CP1253 where

import Data.Encoding.Helper.Template (makeISOInstance)

$( makeISOInstance "CP1253" "CP1253.TXT" )