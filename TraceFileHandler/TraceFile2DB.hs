{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module TraceFile2DB where



import Data.Conduit (($$))
import Data.Conduit.List as CL

--import Database.Esqueleto

import Database.Persist
--import Database.Persist (insertMany, selectList)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH     (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql    (insert)


import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class  (liftIO)
import TraceFileParser hiding (main)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|

TraceRecordDB
   time    Double
   level   Text
   process Int
   thread  Int
   traceType Text
   message Text
   deriving Show
|]

{-
TraceRecord { recordTime :: LocalTime
           , traceLevel   :: TraceLevel
           , processId :: ProcessId
           , threadId :: ThreadId
           , traceType :: TraceType
           , traceMessage   :: TraceMessage
             }
-}

traceToTraceRecordDB :: TraceRecord -> TraceRecordDB
traceToTraceRecordDB record@(TraceRecord t l p h y m) = TraceRecordDB (localTimeToEpochSeconds t) l p h y m

traceToTraceRecordDB2 :: [TraceRecord] -> [TraceRecordDB]
traceToTraceRecordDB2 = fmap traceToTraceRecordDB


--- read file to db


    --return $ fmap traceToTraceRecordDB2 traces --[traceToTraceRecordDB t | t <- traces]

--utcTimeToEpochSeconds :: LocalTime -> Data.Fixed.Pico
--utcTimeToEpochSeconds u = todSec . localTimeOfDay . utcToLocalTime utc

utcTimeToEpochSeconds :: UTCTime -> Double
utcTimeToEpochSeconds u = realToFrac (utcTimeToPOSIXSeconds u)

localTimeToEpochSeconds :: LocalTime -> Double
localTimeToEpochSeconds l = utcTimeToEpochSeconds (localTimeToUTC utc l)

readTracesFromFileDB :: FilePath -> IO [TraceRecordDB]
--readTracesFromFileDB :: FilePath -> IO (Either String [TraceRecordDB])
readTracesFromFileDB p = do
    traces <- readTracesFromFile p
    case traces of
      Right ts ->
        return $ traceToTraceRecordDB2 ts
      Left _ ->
        return $ []

------------------------------------------------------------------
----------- Database ---------------------------------------------
------------------------------------------------------------------

insertRecodsToSqliteDB :: [TraceRecordDB] -> FilePath -> IO [Key TraceRecordDB]
insertRecodsToSqliteDB records dbf = runSqlite (T.pack dbf) $ do
    runMigrationSilent migrateTables
    forM records insert

--insertRecods :: String -> IO [key TraceRecordDB]
insertRecordsFromFile :: FilePath -> FilePath -> IO [()]
insertRecordsFromFile p dbf = do
  mtraces <- readTracesFromFileDB p
  keys <- insertRecodsToSqliteDB mtraces dbf
  forM keys print

dbFilePath = "test1.db" -- :: FilePath 
record1 = TraceRecordDB 2 "1" 2 3 "type2" "Basic tracing"

main :: IO ()
main = do
    let dbf = T.unpack dbFilePath
    keys <- insertRecordsFromFile traceFile dbf
    print "Done main"

readAllFromDB :: IO [Entity TraceRecordDB]
readAllFromDB = runSqlite dbFilePath $ do
          --buildDb
          selectList [] []


readAllPersonNamesFromDB :: IO (String, String)
readAllPersonNamesFromDB = do
          xs <- readAllFromDB
          return ("first", "last")