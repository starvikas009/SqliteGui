module ListBoxEventHandlerV where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import StringHelperV as SHV


createSimpleListBoxDisplay :: Ord a => Behavior [a] -> (a -> String) -> UI (UI.ListBox a)
createSimpleListBoxDisplay bFillItemList fdisplay = UI.listBox
                        bFillItemList  -- pages/rows to display
                        (pure Nothing) -- row Selection by default set to none.
                        (pure $ \x -> UI.span # set text (fdisplay x)) -- x :: DataItem, convert DataItem to String for display

createSimpleListBox :: (Show a, Ord a) => Behavior [a] -> UI (UI.ListBox a)
createSimpleListBox bFillItemList = createSimpleListBoxDisplay bFillItemList SHV.displaySimple


createSimpleListBoxFromEventHandler :: (Show a, Ord a) => IO (Behavior [a], Handler [a], UI (UI.ListBox a))
createSimpleListBoxFromEventHandler = do
                                        (bFillItemList, doFillItemList) <- createFillItemListBehaviorAndHandler
                                        let uiListBox = createSimpleListBox  bFillItemList
                                        return (bFillItemList, doFillItemList, uiListBox)


createSimpleListBoxFromEventHandlerInitialList :: (Show a, Ord a) => [a] -> IO (Behavior [a], Handler [a], UI (UI.ListBox a))
createSimpleListBoxFromEventHandlerInitialList xs = do
                                        (bFillItemList, doFillItemList) <- createFillItemListBehaviorAndHandlerInitialList xs
                                        let uiListBox = createSimpleListBox  bFillItemList
                                        return (bFillItemList, doFillItemList, uiListBox)


createFillItemListBehaviorAndHandler :: IO (Behavior [a], Handler [a])
createFillItemListBehaviorAndHandler = createFillItemListBehaviorAndHandlerInitialList []

createFillItemListBehaviorAndHandlerInitialList :: a -> IO (Behavior a, Handler a)
createFillItemListBehaviorAndHandlerInitialList initialItemList = do
                                      (eFillItemList, doFillItemList) <- newEvent
                                      bFillItemList <- stepper initialItemList eFillItemList
                                      return (bFillItemList, doFillItemList)




{-

listBox ::
  Ord a =>
  Behavior [a]
  -> Behavior (Maybe a)
  -> Behavior (a -> UI Element)
  -> UI (ListBox a)
    -- Defined in ‘Graphics.UI.Threepenny.Widgets’

newEvent :: IO (Event a, Handler a)

type Handler a = a -> IO ()  -- Defined in ‘Reactive.Threepenny’

    lbItemsList <- listBox
        bFillItemList  -- pages/rows to display
        (pure Nothing) -- row Selection by default
        (pure $ \x -> UI.span # set text (displayDataItem x)) -- x :: DataItem, convert DataItem to String for display

-}
