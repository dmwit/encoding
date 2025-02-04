module Main (main) where

import Options.Applicative
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)

data Input = Input 
  { inputDir :: FilePath
  , extensions :: [String]
  } deriving (Show)

input :: Parser Input
input = Input 
  <$> strArgument (metavar "INPUT_DIR" <> help "Input directory with files ...")
  <*> some 
      ( strOption 
        ( short 'e' 
        <> metavar "EXTENSIONS" 
        <> help "Extensions ..."
        )
      )

supportedExtensions :: [String]
supportedExtensions = [".mapping", ".xml"]

filesWithExtensions :: [FilePath] -> [String] -> [FilePath]
filesWithExtensions files exts = filter (\file -> takeExtension file `elem` exts) files

generateEncodings :: Input -> IO ()
generateEncodings Input { inputDir = inputDir, extensions = extensions } = do
  if not (validateExtensions extensions)
    then error 
      ( "Got unexpected extensions: " 
      ++ show extensions 
      ++ ", supported extensions: " 
      ++ show supportedExtensions
      )
    else do
      isDirExist <- doesDirectoryExist inputDir
      if not isDirExist
        then error ("Directory " ++ show inputDir ++ " does not exist")
        else do 
          files <- listDirectory inputDir
          print $ filesWithExtensions files extensions
  where
    validateExtensions = all (`elem` supportedExtensions)

main :: IO ()
main = generateEncodings =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
      <> progDesc "Program description"
      <> header "Header"
      )
