module Main (main) where

import Data.Encoding.Preprocessor.Mapping 
import Data.Encoding.Preprocessor.XMLMappingBuilder
import Data.List (intercalate)
import qualified Data.Map as M
import Distribution.Simple.PreProcess
import Distribution.Verbosity (normal)
import Options.Applicative
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))
import Utils (filesWithExtensions, validateExtensions, withDirectory)

data Input = Input 
  { inputDir :: FilePath
  , extensions :: [String]
  } deriving (Show)

extensionMap :: M.Map String PreProcessor
extensionMap = M.fromList 
  [ (".mapping", mappingPreprocessor)
  , (".mapping2", mappingPreprocessor)
  , (".xml", xmlPreprocessor)
  ]

input :: Parser Input
input = Input 
  <$> strArgument (metavar "INPUT_DIR" <> help "Directory with mapping files")
  <*> some 
      ( strOption 
        ( short 'e' 
        <> metavar "EXTENSIONS" 
        <> help ("Supported extensions: " ++ intercalate ", " (M.keys extensionMap))
        )
      )

generateEncodings :: Input -> IO ()
generateEncodings Input { inputDir = inputDir, extensions = extensions } = do
  validateExtensions extensions (M.keys extensionMap)
  withDirectory inputDir
    ( \dir -> do
      directoryFiles <- listDirectory dir
      let files = filesWithExtensions directoryFiles extensions
      mapM_
        ( \fileName ->
          runSimplePreProcessor
            (getPreProcessor fileName)
            (dir </> fileName)
            (dir </> (replaceExtension fileName ".hs"))
            normal
        )
        files
    )
  where
    getPreProcessor filePath = extensionMap M.! (takeExtension filePath)

main :: IO ()
main = generateEncodings =<< execParser opts
  where
    opts = info (input <**> helper) (fullDesc <> progDesc "Generate encodings")
