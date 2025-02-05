module Main (main) where

import Data.Encoding.Preprocessor.Mapping 
import Data.Encoding.Preprocessor.XMLMappingBuilder
import Data.List (intercalate)
import qualified Data.Map as M
import Distribution.Simple.PreProcess
import Distribution.Verbosity (normal)
import Options.Applicative
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))

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

filesWithExtensions :: [FilePath] -> [String] -> [FilePath]
filesWithExtensions files exts = filter (\filePath -> takeExtension filePath `elem` exts) files

generateEncodings :: Input -> IO ()
generateEncodings Input { inputDir = inputDir, extensions = extensions } = do
  if not (validateExtensions extensions)
    then error 
      ( "Got unexpected extensions: " 
      ++ intercalate ", " extensions 
      ++ ", supported extensions: " 
      ++ intercalate ", " (M.keys extensionMap)
      )
    else do
      isDirExist <- doesDirectoryExist inputDir
      if not isDirExist
        then error ("Directory " ++ show inputDir ++ " does not exist")
        else do 
          directoryFiles <- listDirectory inputDir
          let files = filesWithExtensions directoryFiles extensions
          mapM_ 
            ( \filePath -> 
              runSimplePreProcessor 
                (getPreProcessor filePath)
                (inputDir </> filePath)
                (inputDir </> (replaceExtension filePath ".hs")) 
                normal
            ) 
            files
  where
    validateExtensions = all (`elem` M.keys extensionMap)
    getPreProcessor filePath = extensionMap M.! (takeExtension filePath)

main :: IO ()
main = generateEncodings =<< execParser opts
  where
    opts = info (input <**> helper) (fullDesc <> progDesc "Generate encodings")
