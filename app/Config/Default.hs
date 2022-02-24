{-|
Module      : Config.Default
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Application's default main config (mainly for initial creation of if missing)
-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Default where

import System.FilePath ( (</>) )
import qualified Data.Map as M
import Data.Text ( pack )
import Monomer ( rgbHex )

import Config
import LogParser.LogEntry


defaultAppConfig :: MainConfig
defaultAppConfig = MainConfig 
    { _acMainWindowDefaultSize  = (800, 600)
    , _acRegularFont            = pack $ fontsPath </> "Roboto-Regular.ttf"
    , _acEmphasizeFont          = pack $ fontsPath </> "Roboto-Bold.ttf"
    , _acTextSize               = 14.0
    , _acTimeColors             = (rgbHex "#F08080", rgbHex "#B08080", rgbHex "#808080")
    , _acTimeColorShifts        = (5, 10)
    , _acExplicitSecs           = 60
    , _acSpacerWidth            = 10
    , _acTimeFieldWidth         = 30
    , _acLogWindows             = 2
    , _acColorSampleText        = "Losing is fun!"
    , _acBgColor                = Nothing
    , _acJobDecorUnderline      = False
    , _acJobDecorFgColor        = Just (rgbHex "#808080")
    , _acJobDecorBgColor        = Nothing
    , _acMatDecorUnderline      = False
    , _acMatDecorFgColor        = Nothing
    , _acMatDecorBgColor        = Just (rgbHex "#C0C0C0")
    , _acDorfDecorUnderline     = True
    , _acDorfDecorFgColor       = Nothing
    , _acDorfDecorBgColor       = Nothing
    , _acPreviousLogEntries     = 10
    , _acLogFilePath            = Nothing
    , _acShowProfession         = True
    , _acShowName               = SNFullName
    }


