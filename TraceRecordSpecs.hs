-- specs.hs
{-# LANGUAGE EmptyDataDecls               #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE TypeFamilies                 #-}

module TraceRecordSpecs where

import Data.Text as T 
-- (Text)
import qualified Database.Persist.TH as TH

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"] [TH.persistLowerCase|

TraceRecordDB
   time    Double
   level   Text
   process Int
   thread  Int
   traceType Text
   message Text
   deriving (Show)
   deriving Eq
   deriving Ord --Ord, Eq)
|]

toText :: TraceRecordDB -> Text
toText record@(TraceRecordDB time level process thread traceType message) =
    T.intercalate "    " [ T.pack (show time), level, T.pack (show process), T.pack (show thread), traceType, message]

showTraceRecordDB :: TraceRecordDB -> String --Text
showTraceRecordDB record@(TraceRecordDB time level process thread traceType message) = unpack $ toText record

