
{-# LANGUAGE OverloadedStrings #-}

module TraceDBConfig where

-- imports
import Data.Text (Text)

--databaseFileName4 = "trace1.sqlite" :: Text
--databaseFileName2 = "trace1.db" :: Text
--databaseFileName3 = "./TraceFileHandler/trace1.db" :: Text --testr2.db
--databaseFileName = "./TraceFileHandler/test1.db" :: Text
databaseFileName = "./TraceFileHandler/testr2.db" :: Text

------------------- WebViewer Config -------------------
defaultPagesViewSize = 10:: Int
defaultRowsViewSize = 20:: Int
defaultNumberOfRowsPerPage = 100 :: Int
