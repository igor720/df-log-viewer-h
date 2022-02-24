{-|
Module      : GUI.Model.App
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

App model definition
-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Model.App where

import Control.Lens.TH ( makeLenses )
import Data.Text ( Text )
import qualified Data.Map as M
import Data.Time ( UTCTime )
import Monomer ( Rect )

import LogParser.LogEntry
import Config
import GUI.Model.LogWindowsDialog
import GUI.Model.ColorsDialog


-- | Log Entry Id
type LEId = Int

-- | Dialogs' Apply flag
type IsDialogApply = Bool

-- | App dialog mode
data DialogMode = DMNone | DMColors | DMWindows
    deriving (Eq, Show)

-- | Log merge mode (LMNo is currently of no use)
data LogMergeMode = LMNo | LMFull | LMLast
    deriving (Eq, Show)

-- | Log entry type with Id and optional time
data LogEntry = LogEntry
    { _leId             :: LEId
    , _leTime           :: Maybe UTCTime
    , _leData           :: LogEntryData
    } deriving (Show, Eq)

data AppModel = AppModel
    { _mainConfig       :: MainConfig
    , _errorMsg         :: Maybe Text
    , _reforMode        :: [Int]            -- ^ reformattingMode layers
    , _dialogMode       :: DialogMode       -- ^ dialog mode
    , _logMergeMode     :: LogMergeMode     -- ^ log merge mode
    , _curTime          :: UTCTime          -- ^ time of last log entry reicived
    , _lastId           :: LEId             -- ^ last log entry Id
    , _logEntries       :: [LogEntry]       -- ^ log entries
    , _logColorDistrib  :: LogColorDistrib  -- ^ log colors distribution
    , _logWindowDistrib :: LogWindowDistrib -- ^ log windows distribution
    , _cModel           :: CDialogModel     -- ^ color configuration model
    , _lwModel          :: LWDialogModel    -- ^ window configuration model
    , _logFilePath      :: FilePath         -- ^ gamelog file path
    , _exePath          :: FilePath         -- ^ working directory path
    } deriving (Show, Eq)

data AppEvent
    = AppErrorShow Text
    | AppErrorClose
    | AppInit
    | AppResize Rect
    | AppReformattingDone
    | AppShowColorConfig
    | AppCloseColorConfigScreen IsDialogApply
    | AppShowWindowConfig
    | AppCloseWindowConfigScreen IsDialogApply
    | AppAddRecord LogEntry
    | AppAddBulkRecords [LogEntry] LEId
    deriving (Show)

makeLenses 'LogEntry
makeLenses 'AppModel



