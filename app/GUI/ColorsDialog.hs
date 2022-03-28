{-|
Module      : GUI.ColorsDialog
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Colors dialog creation and functionality
-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.ColorsDialog where

import Control.Lens
import Data.Text (Text)
import TextShow
import qualified Data.Text as T
import qualified Data.Map as M
import Monomer

import LogParser.LogEntry
import GUI.Model.ColorsDialog
import GUI.Model.App

{-# INLINE logWindowBgColor #-}
{-# INLINE defaultColor #-}

logWindowBgColor :: Color
logWindowBgColor = black

defaultColor :: Color
defaultColor = white

buildColorsDialogComp :: Text -> WidgetEnv CDialogModel CDialogEvent 
    -> CDialogModel -> WidgetNode CDialogModel CDialogEvent
buildColorsDialogComp colorSampleText wenv model = widgetTree where
    leTegLabel xt = label (T.drop 2 $ showt xt) `styleBasic`
        [ textFont "Bold", textSize 12, textColor black
        , padding 2 ]
    sampleLabel c = box_ [alignMiddle] $ label colorSampleText `styleBasic`
        [ textSize 12, textColor c, bgColor logWindowBgColor
        , padding 4, maxWidth 106 ]
    colorRow (tag, c) = row where
        row = box $ vstack
            [ hstack [sampleLabel c, spacer, leTegLabel tag]
            , hstack [colorPickerV c (ColorChange tag) `styleBasic`
                [ bgColor logWindowBgColor ]]
            , spacer_ [width 8]
            ]
    cDistribSize = M.size (model^.cDistrib)
    (cLst0, cLst12) = splitAt (cDistribSize `div` 3) $ 
        M.toList (model^.cDistrib)
    (cLst1, cLst2) = splitAt (cDistribSize `div` 3) cLst12
    widgetTree = box (
        hstack
            [ vscroll (vstack (
                over mapped colorRow cLst0
                ) ) `styleBasic` vsStyle
            , spacer
            , vscroll (vstack (
                over mapped colorRow cLst1
                ) ) `styleBasic` vsStyle
            , spacer
            , vscroll (vstack (
                over mapped colorRow cLst2
                ) ) `styleBasic` vsStyle
            ] `styleBasic` hsStyle
        ) `styleBasic` boxStyle
        where
            vsStyle = [ bgColor lightGray
                      , width 310, padding 10, radius 10]
            hsStyle = [ border 1 lightGray, padding 8 ]
            boxStyle = [ paddingH 2, paddingB 2 ]

handleEventColorsDialog :: WidgetEnv CDialogModel CDialogEvent
    -> WidgetNode CDialogModel CDialogEvent -> CDialogModel -> CDialogEvent 
    -> [EventResponse CDialogModel CDialogEvent sp ep]
handleEventColorsDialog wenv node model evt = case evt of
    ColorChange xt c -> [ Model $ model
        & cDistrib . ix xt .~ c
        ]

colorsDialogWidget :: (WidgetModel sp, WidgetEvent ep) => ALens' sp CDialogModel 
    -> Text -> WidgetNode sp ep
colorsDialogWidget field colorSampleText = 
    composite "colorsDialogWidget" field 
        (buildColorsDialogComp colorSampleText) handleEventColorsDialog

defaultColorDistrib :: LogColorDistrib
defaultColorDistrib = distr where
    tags = enumFrom minBound :: [LogEntryTag]
    distr = M.fromList $ zip tags (repeat defaultColor)


