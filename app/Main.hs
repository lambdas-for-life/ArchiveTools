module Main where
import ArchiveTools 
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)

data ArchiveOpts =
  ArchiveOpts {
    searchPaths :: [FilePath]
  , quiet       :: Bool
  }

archiveOpts :: Parser ArchiveOpts
archiveOpts = ArchiveOpts
  <$> some (argument str 
    ( metavar "DIRECTORIES...")
    ) 
  <*> switch
    ( long "quiet"
   <> short 'q'
   <> help "whether to be quiet" )

main :: IO ()
main = runArchiver =<< execParser opts
  where
    opts = info (archiveOpts <**> helper)
      ( fullDesc
     <> progDesc "Search in DIRECTORIES... for directories containing Ableton Live Sets"
     <> header "ArchiveTools-exe - A tool for archiving Ableton sets" )

runArchiver :: ArchiveOpts -> IO ()
runArchiver ao = do
  containers <- mapM listAbletonContainers $ searchPaths ao
  mapM_ print $ join containers
