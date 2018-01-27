module Main where

import           Data.Encoding.Preprocessor.Mapping
import           Data.Encoding.Preprocessor.XMLMappingBuilder
import           Distribution.Simple

main :: IO ()
main = do
  let mp  = ("mapping", \_ _ _ -> mappingPreprocessor)
  let mp2 = ("mapping2",\_ _ _ -> mappingPreprocessor)
  let xp  = ("xml",\_ _ _ -> xmlPreprocessor)
  defaultMainWithHooks $ simpleUserHooks { hookedPreProcessors = mp:mp2:xp:hookedPreProcessors simpleUserHooks }

