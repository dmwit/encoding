{-# LANGUAGE CPP #-}

module Main where

import Distribution.Simple
import Data.Encoding.Preprocessor.Mapping
import Data.Encoding.Preprocessor.XMLMappingBuilder

#if MIN_VERSION_Cabal(2,0,0)
main = defaultMainWithHooks (simpleUserHooks
                             {hookedPreProcessors = ( ("mapping" , \_ _ _ -> mappingPreprocessor)
                                                    : ("mapping2", \_ _ _ -> mappingPreprocessor)
                                                    : ("xml"     , \_ _ _ -> xmlPreprocessor)
                                                    : (hookedPreProcessors simpleUserHooks)
                                                    )
                             })
#else
main = defaultMainWithHooks (simpleUserHooks
                             {hookedPreProcessors = ( ("mapping" , \_ _ -> mappingPreprocessor)
                                                    : ("mapping2", \_ _ -> mappingPreprocessor)
                                                    : ("xml"     , \_ _ -> xmlPreprocessor)
                                                    : (hookedPreProcessors simpleUserHooks)
                                                    )
                             })
#endif
