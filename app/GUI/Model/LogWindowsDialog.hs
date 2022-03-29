{-|
Module      : GUI.Model.WindoesDialog
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Log windows configuration dialog model definition
-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Model.LogWindowsDialog where

import Control.Lens.TH ( makeLenses )
import qualified Data.Map as M

import LogParser.LogEntry


type LogWindow = Int

-- | Log window for LogEntryTag distribution mapping
type LogWindowDistrib = M.Map LogEntryTag LogWindow

newtype LWDialogModel = LWDialogModel {
    _lwLists :: [[LogEntryTag]]
    } deriving (Eq, Show)

data LWDialogEvent
    = DropTo Int LogEntryTag
    deriving (Eq, Show)

makeLenses 'LWDialogModel

  