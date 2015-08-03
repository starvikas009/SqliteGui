-- selectList.hs
{-# LANGUAGE OverloadedStrings #-}

module TraceListFromDB where

import Control.Monad (mapM_, mapM)
import Control.Monad.IO.Class  (liftIO)
import Data.Text (Text, concat)

--import Data.Text as T
import Database.Persist
--- (count, selectList, entityVal, Entity)
import Database.Persist.Sqlite (runSqlite)
import TraceRecordSpecs as S
import TraceDBConfig as Conf
import QueryParser as QP
import Data.Either (either)

--- (myRecordValue)

databaseFile = Conf.databaseFileName

extractRecord :: Entity TraceRecordDB -> TraceRecordDB
extractRecord = entityVal


getRowsCount :: IO (Int)
getRowsCount = runSqlite databaseFile $ do

    -- Select all records from DB
    records <- selectList [] []

    --mapM extractRecord records
    let extractedRows = map extractRecord records
    return $ length extractedRows

{-
selectList ::
  (Control.Monad.IO.Class.MonadIO m, PersistEntity val,
   PersistQuery backend, PersistEntityBackend val ~ backend) =>
  [Filter val]
  -> [SelectOpt val]
  -> Control.Monad.Trans.Reader.ReaderT backend m [Entity val]
    -- Defined in ‘persistent-2.1.1.4:Database.Persist.Class.PersistQuery’
-}
getAllRecords :: IO [TraceRecordDB]
getAllRecords = runSqlite databaseFile $ do

    -- Select all records from DB
    records <- selectList [] []

    --mapM extractRecord records
    let values = map extractRecord records
    return values


getRecordsForPage :: (Int -> Int -> IO [TraceRecordDB])
getRecordsForPage offset resultsPerPage = runSqlite databaseFile $ do
    records <- selectList [] [ LimitTo resultsPerPage , OffsetBy offset]
    let extractedRows = map extractRecord records
    return $ extractedRows

----------------------------------------------------------------------------
-------------------     Filter   -------------------------------------------
----------------------------------------------------------------------------

getFilteredRecords :: Text -> IO [TraceRecordDB]
getFilteredRecords q = runSqlite databaseFile $ do
    -- Select filtered records from DB
    records <- selectList (createFilterListFromQuery q) []
    let values = map extractRecord records
    return values


getRecordsForPageFiltered :: (Text -> Int -> Int -> IO [TraceRecordDB]) --}
getRecordsForPageFiltered q offset resultsPerPage = runSqlite databaseFile $ do
    records <- selectList (createFilterListFromQuery q) [LimitTo resultsPerPage , OffsetBy offset]
    let extractedRows = map extractRecord records
    return $ extractedRows
    

createFilterListFromQuery :: Text -> [Filter TraceRecordDB]
createFilterListFromQuery "" = [] -- for empty string use
createFilterListFromQuery q = [FilterOr [createLikeFilter TraceRecordDBTraceType q, createLikeFilter TraceRecordDBMessage q]] -- 

createPrefixFilter :: EntityField record Text -> Text -> Filter record
createPrefixFilter field val = Filter field (Left $ Data.Text.concat [val, "%"]) (BackendSpecificFilter "LIKE")

createLikeFilter ::  EntityField record Text -> Text -> Filter record
createLikeFilter field val = Filter field (Left $ Data.Text.concat ["%", val, "%"]) (BackendSpecificFilter "LIKE")

----------------------------------------------------------------------------
--------------------  Use Parser
----------------------------------------------------------------------------

getRecordsForPageParsedQuery :: (Text -> Int -> Int -> IO [TraceRecordDB]) --}
getRecordsForPageParsedQuery q offset resultsPerPage = runSqlite databaseFile $ do
    let dbfilters = createDBFilterFromParsedQueryHandleError q
    records <- selectList dbfilters [LimitTo resultsPerPage , OffsetBy offset]
    let extractedRows = map extractRecord records
    return $ extractedRows

createDBFilterFromParsedQueryHandleError :: Text -> [Filter TraceRecordDB]
createDBFilterFromParsedQueryHandleError input = createDBFilterFromParsedQuery cfs2
                                                    where cfs1 = runCombinedFilterParserOnQuery input
                                                          cfs2 = either (\x -> []) (id) cfs1

createDBFilterFromParsedQuery :: [CombinedFilter] -> [Filter TraceRecordDB]
createDBFilterFromParsedQuery fs = (FilterAnd $ fmap createDBFilter1 fs) : []

createDBFilter1 :: CombinedFilter -> Filter TraceRecordDB
createDBFilter1 (CombinedFilterType (TypeContains q))       = createLikeFilter TraceRecordDBTraceType q
createDBFilter1 (CombinedFilterMessage (MessageContains q)) = createLikeFilter TraceRecordDBMessage q
createDBFilter1 (CombinedFilterLevel (LevelFilterQCtor ls)) = FilterOr $ fmap createLevelFilter ls

createLevelFilter :: LevelT -> (Filter TraceRecordDB)
createLevelFilter LevelI = createPrefixFilter TraceRecordDBLevel "I"
createLevelFilter LevelW = createPrefixFilter TraceRecordDBLevel "W"
createLevelFilter LevelE = createPrefixFilter TraceRecordDBLevel "E"


main1 :: IO ()
main1 = runSqlite databaseFile $ do

    -- Select all records from DB and unpack into a list of Ints
    records <- selectList [] []
    let values = map (traceRecordDBMessage . extractRecord) records
    mapM_ (liftIO . putStrLn . show) $ zip values $ tail values
    