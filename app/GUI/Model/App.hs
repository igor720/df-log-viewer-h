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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module GUI.Model.App where

import Control.Lens
--import Control.Lens.TH ( makeLenses )
import Data.Text ( Text )
import qualified Data.IntMap.Strict as IM
import Data.Time ( UTCTime )
import Monomer ( Rect )
import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )

--import LogParser.LogEntry
import LogParser.Reassemble
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
-- TODO: currently LMLast is the same as LMFull
data LogMergeMode = LMNo | LMFull | LMLast
    deriving (Eq, Show)

-- | Log entry type with Id and optional time
data LogEntry = LogEntry
    { _leId             :: !LEId
    , _leTime           :: Maybe UTCTime
    , _leReLogEntry     :: !ReLogEntry
    } deriving (Show, Eq, Generic, NFData)

type LogEntriesIndex = Int

-- | In memory repository for log entries
data LogEntriesDepository = LogEntriesDepository
    { _lesSize          :: !LogEntriesIndex
    , _lesData          :: IM.IntMap LogEntry
    } deriving (Show, Eq, Generic, NFData)

makeLenses 'LogEntriesDepository

-- | Below are functions for log entries depository manipulation
emptyLogEntriesDepository :: LogEntriesDepository
emptyLogEntriesDepository = LogEntriesDepository 0 IM.empty

makeLogEntriesDepositoryFromBulk :: [LogEntry] -> LogEntriesDepository
makeLogEntriesDepositoryFromBulk les =
    LogEntriesDepository (length les) (IM.fromList $ zip [1..] (reverse les))

addToLogEntriesDepository :: LogEntriesIndex -> LogEntry -> LogEntriesDepository 
        -> LogEntriesDepository
addToLogEntriesDepository maxSize le (LogEntriesDepository idx dt)
    | idx<maxSize   = LogEntriesDepository (idx+1)
                        (IM.insert (idx+1) le dt)
    | otherwise     = LogEntriesDepository (idx+1)
                        (IM.insert (idx+1) le $ IM.delete (idx-maxSize+1) dt)

getLogEntries :: LogEntriesDepository -> [LogEntry]
getLogEntries depos = map snd $ reverse $ IM.toList (depos^. lesData)

-- | Monomer application model
data AppModel = AppModel
    { _mainConfig       :: MainConfig
    , _errorMsg         :: Maybe Text
    , _reforMode        :: [Int]            -- ^ reformattingMode layers
    , _dialogMode       :: DialogMode       -- ^ dialog mode
    , _logMergeMode     :: LogMergeMode     -- ^ log merge mode
    , _curTime          :: UTCTime          -- ^ time of last log entry reicived
    , _lastId           :: LEId             -- ^ last log entry Id
    , _logEntries       :: LogEntriesDepository -- ^ log entries depository
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
    | AppWindowSizeSaved
    | AppReformattingDone
    | AppShowColorConfig
    | AppCloseColorConfigScreen IsDialogApply
    | AppColorConfigSaved
    | AppShowWindowConfig
    | AppCloseWindowConfigScreen IsDialogApply
    | AppWindowConfigSaved
    | AppAddRecord LogEntry
    | AppSetFocus Text
    | AppAddBulkRecords [LogEntry] LEId
    deriving (Show)

makeLenses 'LogEntry
makeLenses 'AppModel










