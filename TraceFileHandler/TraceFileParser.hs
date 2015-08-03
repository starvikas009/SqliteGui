{-
    parsing trace file
-}

{-# LANGUAGE OverloadedStrings #-}

module TraceFileParser where

import Data.Word
import Data.Time
import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.ByteString as B
--import Data.Attoparsec.Text
import Data.Text (Text)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as L


-----------------------
-------- TYPES --------
-----------------------

type ProcessId = Int
type ThreadId = Int
type TraceLevel = Text --B.ByteString
type TraceType = Text --B.ByteString
type TraceMessage = Text -- B.ByteString

data TraceRecord =
  TraceRecord { recordTime :: LocalTime
           , traceLevel   :: TraceLevel
           , processId :: ProcessId
           , threadId :: ThreadId
           , traceType :: TraceType
           , traceMessage   :: TraceMessage
             } deriving Show

type TraceList = [TraceRecord]

-- | trace sub part delemeter
partDelemeter = ',' --','

-----------------------
------- PARSING -------
-----------------------

-- | Parser of values of type 'LocalTime'.
isDigit c = c >= '0' && c <= '9'

digitsParser :: Parser Text
digitsParser = takeWhile1 (isDigit)

traceTimeParser :: Parser LocalTime
traceTimeParser = do
  y  <- many1 digit --digitsParser
  char '-'
  mm <- many1 digit 
  char '-'
  d  <- many1 digit 
  char ' '
  h  <- many1 digit
  char ':'
  m  <- many1 digit
  char ':'
  s  <- many1 digit
  char '.'
  fff <- many1 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read (s ++ "." ++ fff))
                }

traceLevelParser :: Parser TraceLevel
traceLevelParser = do
    l <- takeTill (\c -> c == partDelemeter)
    return $ l

processIdParser :: Parser ProcessId
processIdParser = do 
    pid <- many1 digit
    return $ read pid

threadIdParser :: Parser ThreadId
threadIdParser = do
    tid <- many1 digit
    return $ read tid


traceTypeParser :: Parser TraceType
traceTypeParser = do
    t <- takeTill (\c -> c == partDelemeter)
    return $ t

traceMessageParser :: Parser TraceMessage
traceMessageParser = do
    msg <- takeTill (isEndOfLine)-- takeText --many anyChar --takeTill (isEndOfLine)
    return $ msg
{-

-}

traceEntryParser :: Parser TraceRecord
traceEntryParser = do
  -- First, we read the time.
  time <- traceTimeParser

  -- Followed by a space.
  char partDelemeter
  -- And then the IP of the client.
  lvl <- traceLevelParser
  -- Followed by another space.
  char partDelemeter
  -- Finally, we read the type of product.
  pid <- processIdParser
  char partDelemeter

  tid <- threadIdParser
  char partDelemeter

  tp <- traceTypeParser
  char partDelemeter

  msg <- traceMessageParser
  -- And we return the result as a value of type 'LogEntry'.
  return $ TraceRecord time lvl pid tid tp msg


traceListParser :: Parser TraceList
traceListParser = many $ traceEntryParser <* endOfLine

--readFileContent = Lazy
--readTracesFromFile :: FilePath -> IO ()
readTracesFromFile :: FilePath -> IO (Either String TraceList)
readTracesFromFile traceFile = do
    content <- L.readFile traceFile
    return $ parseOnly traceListParser content
    
----------------------
-------- TEST --------
----------------------
exampleLog2 = "2014-6-9 01:34:26.463,Informational,3580,3584,General.FabricSetup,Setting up crash dump directory result: true"

traceFile = "traceExample.txt"


main :: IO ()
main = do
    print $ parseOnly traceTimeParser "2014-6-9 01:34:26.463"
    print $ parseOnly traceEntryParser exampleLog2
    print $ (read "2014-06-09 01:34:26.463" :: LocalTime) 
    --print $ parseOnly traceListParser (getContents )
    L.readFile traceFile >>= print . parseOnly traceListParser