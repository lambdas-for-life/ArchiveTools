{-# LANGUAGE OverloadedStrings #-}
module ListAbletonContainers
    ( listAbletonContainers
    ) where
import Data.Text        ( isPrefixOf
                        , isSuffixOf
                        , pack)
import System.Directory ( doesDirectoryExist
                        , listDirectory
                        , makeAbsolute
                        , withCurrentDirectory)
import Control.Monad    ( void
                        , filterM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)

--Run recursive writer of ableton directories
listAbletonContainers :: FilePath -> IO [FilePath]
listAbletonContainers dir =
  snd <$> runWriterT (writeListAbletonContainers dir)

-- Recurse through all subdirectories
-- Write paths of directories which contain files with extension .als
writeListAbletonContainers :: FilePath -> WriterT [FilePath] IO ()
writeListAbletonContainers dir = do
  subdirs <- lift $ listAllSubdirectories dir
  containers <- lift $ filterM containsAbletonLiveSet subdirs
  tell containers
  lift $ mapM (\c -> print $ "found: " ++ c) containers
  void $ mapM writeListAbletonContainers subdirs

-- Returns true if path is a directory containing a file
-- with extension .als
containsAbletonLiveSet :: FilePath -> IO Bool
containsAbletonLiveSet dir = do
  ls <- listDirectory dir
  return $ any isAbletonLiveSet ls
      
-- Returns true if path suffix is .als
isAbletonLiveSet :: FilePath -> Bool
isAbletonLiveSet = isSuffixOf ".als" . pack

-- Returns absolute paths of subdirectories of given path
-- Safe to use from different working directory  
listAllSubdirectories :: FilePath -> IO [FilePath]
listAllSubdirectories dir = withCurrentDirectory dir $ do
  l <- stripDotUnderscoreFiles <$> listDirectory dir
  filterM doesDirectoryExist =<< mapM makeAbsolute l

-- Strip dot underscore prefixed files that Mac creates for obscure reasons
-- e.g. ._SubDirectoryName in same directory as SubDirectoryName
-- These are housekeeping files for Mac filesystem and probably safe to ignore
-- when archiving as they won't contain any of 'our' data
stripDotUnderscoreFiles :: [FilePath] -> [FilePath]
stripDotUnderscoreFiles = filter (not . isPrefixOf "._" . pack)

