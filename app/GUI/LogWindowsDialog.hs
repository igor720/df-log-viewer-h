{-|
Module      : GUI.LogWindowsDialog
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Log windows dialog creation and functionality
-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.LogWindowsDialog where

import Control.Lens
import TextShow
import qualified Data.List as List

import qualified Data.Text as T
import qualified Data.Map as M
import Monomer

import LogParser.LogEntry
import GUI.Model.LogWindowsDialog
import GUI.Model.App

{-# INLINE listingBgColor #-}
{-# INLINE listingHoverColor #-}
{-# INLINE textDragColor #-}
{-# INLINE defaultLogWindow #-}

listingBgColor :: Color
listingBgColor      = rgbHex "#80B6FD"

listingHoverColor :: Color
listingHoverColor   = rgbHex "#A0D8FD"

textDragColor :: Color
textDragColor       = rgbHex "#E0FFFF"

defaultLogWindow :: LogWindow
defaultLogWindow    = 4

buildLogWindowsDialogComp :: WidgetEnv LWDialogModel LWDialogEvent 
    -> LWDialogModel -> WidgetNode LWDialogModel LWDialogEvent
buildLogWindowsDialogComp wenv model = widgetTree where
    itemA val = box_ [alignLeft] $ label (T.drop 2 $ showt val) `styleBasic`
        [ textColor black, padding 5 ]
    dragItem val = draggable_ val
        [draggableStyle [ bgColor textDragColor, radius 5 ]]
        (itemA val) `styleBasic` [ cursorHand ]
    dragList items = vscroll $ vstack (dragItem <$> items)
    dropContainer target iList = dropTarget_ target
        [dropTargetStyle [ bgColor listingHoverColor, radius 10 ]]
        (dragList (model^.lwLists.ix iList)) `styleBasic`
            [ bgColor listingBgColor
            , minWidth 200, minHeight 100, padding 5, radius 10]
    dropTarget0 = dropContainer (DropTo 0) 0
    dropTarget1 = dropContainer (DropTo 1) 1
    dropTarget2 = dropContainer (DropTo 2) 2
    dropTarget3 = dropContainer (DropTo 3) 3
    dropTarget4 = dropContainer (DropTo 4) 4
    widgetTree = box ( 
        hstack [
            vstack [dropTarget0] `styleBasic` vsStyle,
            spacer,
            vstack [dropTarget1, spacer, dropTarget2] `styleBasic` vsStyle,
            spacer,
            vstack [dropTarget3, spacer, dropTarget4] `styleBasic` vsStyle
            ] `styleBasic` hsStyle
        ) `styleBasic` boxStyle
        where 
            vsStyle = [ minWidth 100 ]
            hsStyle = [ border 1 lightGray
                      , maxWidth 938, paddingH 8, paddingV 8 ]
            boxStyle = [ paddingH 2, paddingB 2 ]

-- Probably, it is exists the possibility (SystemEvent?) to delete entry 
-- only from one list, but at this moment I don't know how to do that.
handleEventLogWindowsDialog :: WidgetEnv LWDialogModel LWDialogEvent 
    -> WidgetNode LWDialogModel LWDialogEvent -> LWDialogModel -> LWDialogEvent 
    -> [EventResponse LWDialogModel LWDialogEvent sp ep]
handleEventLogWindowsDialog wenv node model evt = case evt of
    DropTo j val -> [ Model $ model
        & lwLists .~ map (\i -> 
            if i/=j then List.delete val (model^.lwLists.ix i) 
                    else List.sort (val : model^.lwLists.ix i) 
            ) [0..(model^.lwLists.to length)-1]
        ]

logWindowsDialogWidget :: (WidgetModel sp, WidgetEvent ep)
    => ALens' sp LWDialogModel -> WidgetNode sp ep
logWindowsDialogWidget field = 
    composite "windowConfigWidget" 
        field buildLogWindowsDialogComp handleEventLogWindowsDialog

defaultLogWindowDistrib :: LogWindowDistrib
defaultLogWindowDistrib = distr where
    tags = enumFrom minBound :: [LogEntryTag]
    distr = M.fromList $ zip tags (repeat defaultLogWindow)

