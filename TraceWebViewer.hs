-- working.. showing a listbox with two rows thats it.
-- showing details of row/trace being selected
-- displaying level/time/warning/..
-- add paging # done
-- 
-- TODO: try thousands of row

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Text (Text)
--import Data.String.Conversions (cs)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Text.Printf
import Safe          (readMay)
import Data.String (fromString)


import TraceRecordSpecs as Spex
import TraceDBConfig as Conf
import TraceListFromDB as TDB
import ListBoxEventHandlerV as LBHV

---------------------- ==================
-----
---------------------- ==================
----- Configuration Paging
---------------------- ==================

-- | Main entry point.
main :: IO ()
main = do
    print ( "\n \n ################### ############### main: Starting \n"  )
    startGUI defaultConfig (setup)

    print ( "\n \n ################### ############### main done \n"  )

setup :: Window -> UI ()
setup window = mdo
    -- active elements
    return window # set title "Database rows displaying Example 8"

    -- {============ VPageList
    -- row count
    -- uiRowCountDisplay <- UI.span # set text ("Total rows: " ++ (show rowCountn))
    initialPageListIO <- liftIO $ getPageListIO ""

    -- ===================== Adding new listBox =========================
    --- -- (Behavior [DataItem], Handler [DataItem], UI (ListBox DataItem))
    -- (bFillItemList11, doFillItemList1, uiListBox11) <- createSimpleListBoxFromEventHandlerInitialList [""]
    let initialPageList = [] :: [PageInfoItem]
    (bFillPageList, doFillPageList, uiPageListBox) <- liftIO $ createListBox initialPageListIO

    let initialItemList = [] :: [DataItem]
    (bFillItemList, doFillItemList, uiItemListBox) <- liftIO $ createListBox initialItemList

    uiPageListBox1 <- uiPageListBox
    uiItemListBox1 <- uiItemListBox

    -- ============= list box =========
    element uiPageListBox1 # set (attr "size") (show pagesViewSize) # set style [("width", "auto")]
    element uiItemListBox1 # set (attr "size") (show rowsViewSize) # set style [("width", "1500px"), ("min-width", "200px")]
    ----(show numberOfRowsPerPage)
    -- ============= list box =========

    -- ============ VPageList }

    -- details of one row
    messageDetails <- UI.input
    element messageDetails # set style [("width", "1400px")]
    ---------------------

    ------- FILTER ----------
    uiBtnFilter     <- UI.button # set UI.text "Filter"
    filterEntry     <- UI.entry    bFilterString
    currentFilter   <- UI.input  # set UI.text "liveDisplay6Out Initial out"
    filterButton    <- UI.button    # set UI.text "Filter"

    ------------------------

    getBody window #+
        [grid
            [
                [row [UI.span # set text "New filter query: ", element filterEntry, element filterButton]]
                ---- TODO add this back once update to input is working.. -- , [row [UI.span # set text "  Current filter: ", element currentFilter]]
                , [row [UI.span # set text "Pages: ", element uiPageListBox1]]
                , [row [UI.span # set text "Trace details: ", element messageDetails]]
                , [row [UI.span # set text "Rows: ", element uiItemListBox1]]
            ]
        ]

    ----------------  List selection story --------------

    let
        eRowSelection :: Event (Maybe DataItem)
        eRowSelection = rumors $ UI.userSelection uiItemListBox1

    --bRowSelection :: Behavior (Maybe DataItem) ??
    bRowSelection <- stepper Nothing $ eRowSelection

    let
        bRowSelectionDisplayString :: Behavior (String)
        bRowSelectionDisplayString = showMaybeDataItem <$> bRowSelection
        --bDataItemSelection = stepper Nothing $ UI.userSelection listBox

    --element messageDetails # sink value bRowSelectionDisplayString

    --let bDataItemRowSelection :: Behavior (Maybe DataItem)
    bDataItemRowSelection <- stepper Nothing $ rumors $ UI.userSelection uiItemListBox1
        
    let
        bDataItemRowSelectionVal :: Behavior String
        bDataItemRowSelectionVal = showMaybeDataItem <$> bDataItemRowSelection

    -- (on) :: (element -> Event a) -> element -> (a -> UI void) -> UI ()
    element messageDetails # sink value bDataItemRowSelectionVal
    ----------------  List selection story --------------}

    -------- Filter story ---------------
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry
    bFilterStringCuted  <- stepper "" (bFilterString <@ UI.click filterButton)

    let updatePageListItemsIO = do
        liftIO $ (getPageListIOb bFilterStringCuted) >>= doFillPageList

    let updateItemListBoxIO pageIndexN = do
        liftIO $ (getItemListFilteredIOb bFilterStringCuted pageIndexN) >>= doFillItemList

    on UI.click filterButton $ const $ do
        updatePageListItemsIO
        element currentFilter # sink UI.text bFilterStringCuted

    ----------------- Page Selection Story --------------
    let --updateItemListBoxOnPageListSelectionChange :: Int -> UI void
        updateItemListBoxOnPageListSelectionChange pageIndexMaybe = do
            let pageIndexN = fromMaybe 0 pageIndexMaybe
            liftIO $ (getItemListFilteredIOb bFilterStringCuted pageIndexN) >>= doFillItemList
            UI.setFocus $ getElement uiPageListBox1

    -- UI.selectionChange :: Element -> Event (Maybe Int)
    let ePageListBoxSelectionChange = on UI.selectionChange (getElement uiPageListBox1)

    ePageListBoxSelectionChange $ updateItemListBoxOnPageListSelectionChange
    ----------------- Page Selection Story } --------------



---------- File Scope ===================================

----- PAGE ---------
type PageInfoItem = String
type PageListQueryInput = String



getPageListIOb :: Behavior PageListQueryInput -> IO [PageInfoItem]
getPageListIOb txtb = do
  txtbv <- currentValue txtb
  putStrLn ("PageListQueryInput getPageListIOb <" ++ (show txtbv) ++ ">")
  getPageListIO $ txtbv

getPageListIO :: PageListQueryInput -> IO [PageInfoItem]
getPageListIO x = do
                let pageSize = numberOfRowsPerPage
                totalRowCount <- liftIO $ getTotalRowCount
                let numberOfPages = divisionCeiling totalRowCount pageSize
                putStrLn ("PageListQueryInput<" ++ x ++ ">")
                return [show i | i <- [1..numberOfPages]]


-- ROW/ RECORD data types
type DataItem = Spex.TraceRecordDB
type DataItemQueryInput = String
showDataItem :: DataItem -> String --Text
showDataItem = showTraceRecordDB

emptyDisplay = "--empty--" 

showMaybeDataItem :: Maybe DataItem -> String
showMaybeDataItem = maybe emptyDisplay showDataItem

getItemListIO :: Int -> IO [DataItem]
getItemListIO = getItemListFilteredIO ""

{-
    let pageSize = numberOfRowsPerPage
    --rows <- TDB.getRecordsForPage ((pageIndex) * pageSize) pageSize
    --writeIORef (liftIO iorefsRowsForOnePage) rows
    --return rows
    return $ (getItemListFilteredIO "" pageIndex)
    --}

getItemListFilteredIOb :: Behavior DataItemQueryInput -> Int -> IO [DataItem]
getItemListFilteredIOb txtb pageIndex = do
    txtbv <- currentValue txtb
    getItemListFilteredIO txtbv pageIndex

getItemListFilteredIO :: DataItemQueryInput -> Int -> IO [DataItem]
getItemListFilteredIO q pageIndex = do
    let pageSize = numberOfRowsPerPage
        qtxt = (fromString q) :: Text
    rows <- TDB.getRecordsForPageParsedQuery qtxt ((pageIndex) * pageSize) pageSize
    putStrLn ("getItemListFilteredIO  <" ++ (show qtxt) ++ ">")
    --writeIORef (liftIO iorefsRowsForOnePage) rows
    return rows

{-
getItemListIOb :: Behavior DataItemQueryInput -> Int -> IO [DataItem]
getItemListIOb txtb pageIndex = do
    let pageSize = numberOfRowsPerPage
    rows <- TDB.getRecordsForPage ((pageIndex) * pageSize) pageSize
    --writeIORef (liftIO iorefsRowsForOnePage) rows
    return rows
--}

numberOfRowsPerPage :: Int -- TODO: Remove it, use input text instead
numberOfRowsPerPage = Conf.defaultNumberOfRowsPerPage

rowsViewSize :: Int
rowsViewSize = Conf.defaultRowsViewSize

pagesViewSize :: Int
pagesViewSize = Conf.defaultPagesViewSize

getTotalRowCount :: IO (Int)
getTotalRowCount = do
    rowcount <- liftIO $ TDB.getRowsCount
    return $ rowcount

createListBox :: (Show a, Ord a) => [a] -> IO (Behavior [a], Handler [a], UI (UI.ListBox a))
createListBox xs = do 
    (bFillItemList, doFillItemList, uiListBox) <- LBHV.createSimpleListBoxFromEventHandlerInitialList xs
    return (bFillItemList, doFillItemList, uiListBox)

recordtest1 = TraceRecordDB 21 "1" 2 3 "type1" "Basic tracing1"

-------------------------

divisionCeiling :: Int -> Int -> Int -- Integral 
divisionCeiling a divider = 
    let f = (fromIntegral a) / (fromIntegral divider)
    in ceiling f

------------- Widget -----


setTextb :: Widget w => w -> Behavior [Char] -> UI Element
setTextb elm txtb = do
  txtbv <- currentValue txtb
  (setText elm) $ txtbv
  {--}


setText :: Widget w => w -> [Char] -> UI Element
setText elm txt = element elm # set UI.text ("setText : " ++ txt)
