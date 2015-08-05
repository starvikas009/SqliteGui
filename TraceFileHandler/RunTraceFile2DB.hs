--- example http://en.literateprograms.org/Word_count_%28Haskell%29
--- example http://leiffrenzel.de/papers/commandline-options-in-haskell.html

------------------------------------------------- USAGE -------------------------------------------------
-- 
--- runhaskell RunTraceFile2DB.hs -l dummyLogFile.txt -d test1.db
----- Above command will parse logfile (dummyLogFile.txt) and insert rows into database file (test1.db)
------------------------------------------------- USAGE -------------------------------------------------

module RunTraceFile2DB where

--import System ( getArgs )

import Control.Monad
import Control.Applicative
import System.Environment ( getArgs )
import System.Directory (doesFileExist)
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import TraceFile2DB as ToDB hiding (main)

main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> handleCommandLineOptions flags -- print $ length flags -- 
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Logfile FilePath | DatabaseFile FilePath

options :: [OptDescr Flag]
options = [
    Option ['l'] ["logfile"]  (ReqArg Logfile "LOG-FILE")      "logfile which will be parsed. Requires a file argument"
    , Option ['d'] ["databasefile"] (ReqArg DatabaseFile "DataBase-FILE")      "database file where logs will be stored from log-file. Requires a file argument"
  ]

header = "Usage: main [OPTION...]"

handleCommandLineOptions :: [Flag] -> IO [()]
handleCommandLineOptions flags = do
  print $ length flags
  let logfile = getLogfile flags
  print $ "logfile: " ++ logfile
  assertFileExists logfile

  let dbfile = getDatabaseFile flags
  print $ "dbfile: " ++ dbfile
  -- assertFileExists dbfile -- dbfile will be created if does not exists
  -- Insert records from file to db.
  ToDB.insertRecordsFromFile logfile dbfile

getLogfile :: [Flag] -> FilePath
getLogfile flags = let flag0 = flags !! 0 in
                    case (flag0) of
                      Logfile f -> f
                      _ -> error $ "Invalid options. First option must be Logfile."


getDatabaseFile :: [Flag] -> FilePath
getDatabaseFile flags = let flag1 = flags !! 1 in
                  case (flag1) of
                    DatabaseFile f -> f 
                    _ -> error $ "Invalid options. Second option must be DatabaseFile."

assertFileExists logfile = do
  exists <- doesFileExist logfile
  if (exists) then print $ "file exists: " ++ logfile
    else error $ "Error: file does not exists: " ++ logfile

--insertRecordsFromFiles :: [FilePath] -> IO ()

