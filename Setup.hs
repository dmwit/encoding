{-# LANGUAGE OverloadedStrings #-}
module Main where

import Distribution.Simple
import Data.Encoding.Preprocessor.Mapping
import Data.Encoding.Preprocessor.XMLMappingBuilder

main = defaultMainWithHooks (simpleUserHooks
                             {hookedPreProcessors = (("mapping",\_ _ _ -> mappingPreprocessor)
                                                     :("mapping2",\_ _ _ -> mappingPreprocessor)
                                                     :("xml",\_ _ _ -> xmlPreprocessor)
                                                     :(hookedPreProcessors simpleUserHooks)
                                                    )
                             })
