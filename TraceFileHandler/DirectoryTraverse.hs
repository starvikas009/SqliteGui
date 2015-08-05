---- http://www.mew.org/~kazu/proj/enumerator/
import Control.Monad
import Control.Applicative
import Data.List
import System.Directory
import System.FilePath

getValidContents :: FilePath -> IO [String]
getValidContents path = 
    filter (`notElem` [".", "..", ".git", ".svn"])
    <$> getDirectoryContents path

isSearchableDir :: FilePath -> IO Bool
isSearchableDir dir =
    (&&) <$> doesDirectoryExist dir
         <*> (searchable <$> getPermissions dir)

isRegFile :: FilePath -> IO Bool
isRegFile path = doesFileExist path

findImperative :: FilePath -> String -> IO ()
findImperative dir pattern = do
  cnts <- map (dir </>) <$> getValidContents dir
  forM_ cnts $ \path -> do
    when (pattern `isInfixOf` path) $ putStrLn path
    isDirectory <- isSearchableDir path
    when isDirectory $ findImperative path pattern


getDirectoryContents1 :: FilePath -> IO [FilePath]
getDirectoryContents1 path = 
    filter (`notElem` [".", ".."])
    <$> getDirectoryContents path


getDirectoryContentPaths :: FilePath -> IO [FilePath]
getDirectoryContentPaths dir = map (dir </>) <$> getDirectoryContents1 dir


getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories dir = getDirectoryContentPaths dir >>= filterM isSearchableDir


getRegularFiles :: FilePath -> IO[FilePath]
getRegularFiles dir = getDirectoryContentPaths dir >>= filterM isRegFile


doesExtMatch :: String -> FilePath -> Bool
doesExtMatch ext path = takeExtension path == ext


-- getFilesWithExt ".db" "."
getFilesWithExt :: String -> FilePath -> IO [FilePath]
getFilesWithExt ext dir = do
    items <- getRegularFiles dir
    let items2 = filter (doesExtMatch ext) items
    return items2


getTraceFilesInDir0 :: FilePath -> IO [FilePath]
getTraceFilesInDir0 = getFilesWithExt traceFileExt


getTraceFilesInDir1 :: FilePath -> IO [FilePath]
getTraceFilesInDir1 dir = do
    folders <- getSubDirectories dir
    files <- mapM getTraceFilesInDir0 folders
    return $ concat files


getTraceFilesInDir2 :: FilePath -> IO [FilePath]
getTraceFilesInDir2 dir = do
    folders <- getSubDirectories dir
    files <- mapM getTraceFilesInDir1 folders
    return $ concat files


concatResult :: [[a]] -> [a]
concatResult xs = concat xs
--concatResult x:xs = x ++ concatResult xs


traceFileExt = ".dtr" :: String


getRegularFiles2 :: FilePath -> IO [FilePath]
getRegularFiles2 path = do
    items <- getDirectoryContents1 path
    --isSearchableDir1 <- (\f -> isSearchableDir f)
    --forM_ items $ \path -> do
    --    when isDirectory <- isSearchableDir path
    --let items2 = [f  items] --items2 <- (filter (isSearchableDir)) $ items
    --items2 <- (filter (isSearchableDir)) <$> items
    return items
    --items2 <- (filter (isSearchableDir)) $ items
    --return items2

    --filter (liftIO $ isSearchableDir)
    -- <$> getContents1 path
{--
filterRegFiles :: [FilePath] -> IO [FilePath]
filterRegFiles fs = do
    items <- (filter <$> isSearchableDir)
        <$> fs
    return items
--filterRegFiles x:xs = 
    -}