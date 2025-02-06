module Utils where

import Control.Monad (when)
import Data.List (intercalate)
import System.Directory (doesDirectoryExist)
import System.FilePath (replaceExtension, takeExtension, (</>))

filesWithExtensions :: [FilePath] -> [String] -> [FilePath]
filesWithExtensions files exts = filter (\filePath -> takeExtension filePath `elem` exts) files

validateExtensions :: [String] -> [String] -> IO ()
validateExtensions extensions supportedExtensions = do
  when (not (validateExtensions' extensions)) $ error
    ( "Got unexpected extensions: "
    ++ intercalate ", " extensions
    ++ ", supported extensions: "
    ++ intercalate ", " supportedExtensions
    )
  return ()
    where
      validateExtensions' = all (`elem` supportedExtensions)

withDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
withDirectory dir action = do
  dirExist <- doesDirectoryExist dir
  when (not dirExist) $ error ("Directory " ++ show dir ++ " does not exist")
  action dir
