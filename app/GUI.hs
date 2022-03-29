{-|
Module      : GUI
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Graphical user integface starting module
-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import System.FilePath ( (</>) )
import Control.Lens
import Data.Maybe ( fromMaybe )
import Data.Time ( getCurrentTime )
import qualified Data.Text as T
import qualified Data.Map as M
import Monomer
import qualified Monomer.Lens as L

import Config
import GUI.Model.ColorsDialog
import GUI.Model.LogWindowsDialog
import GUI.Model.App
import GUI.App
import GUI.ColorsDialog
import GUI.LogWindowsDialog

-- | Starts application GUI
gui :: FilePath -> MainConfig -> AppWindowSize -> FilePath -> IO ()
gui path cfg aws logpath = do
    time0 <- getCurrentTime
    logColors <- readColorConfig path
    logWindows <- readWindowConfig path
    let appConfig = [
            appWindowTitle "DFLogViewerH",
            appWindowState (MainWindowNormal aws),
            appTheme (customDarkTheme cfg),
            appFontDef "Regular" $ T.pack $ 
                path </> fontsPath </> T.unpack (cfg^. acRegularFont),
            appFontDef "Bold" $ T.pack $ 
                path </> fontsPath </> T.unpack (cfg^. acEmphasizeFont),
            appInitEvent AppInit,
            appResizeEvent AppResize
            ]
        initLogEntries = emptyLogEntriesDepository -- []
        colorsModel = CDialogModel {
            _cDistrib = M.empty
        }
        windowsModel = LWDialogModel {
            _lwLists = [[], [], [], [], []]
            }
        initModel = AppModel 
                cfg
                Nothing                 -- no error message
                []                      -- no reformattingMode layers
                DMNone                  -- no dialog
                LMFull                  -- full merge for app start
                time0                   -- current time (app start)
                0                       -- start log entry Id
                initLogEntries          -- init log entries
                (M.union logColors defaultColorDistrib)
                (M.union logWindows defaultLogWindowDistrib)
                colorsModel 
                windowsModel 
                logpath
                path
    startApp initModel handleEvent buildUI appConfig

customDarkTheme :: MainConfig -> Theme
customDarkTheme cfg = darkTheme
    & L.userColorMap . at "bgColor" 
        ?~ fromMaybe (darkTheme^. L.clearColor) (cfg^.acBgColor)
    & L.userColorMap . at "hoverBgColor" 
        ?~ fromMaybe (darkTheme^. L.clearColor) (cfg^.acBgColor)
    & L.basic . L.labelStyle .~  textSize (cfg^.acTextSize)



