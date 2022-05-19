{-|
Module      : GUI.Model.ColorsDialog
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Colors configuration dialog model definition
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GUI.Model.ColorsDialog where

import Control.Lens.TH ( makeLenses )
import qualified Data.Map as M
import Monomer ( Color(Color) )

import LogParser.LogEntry


-- | For easy 'Color' parsing
deriving instance Read Color

-- | Color for LogEntryTag distribution mapping
type LogColorDistrib = M.Map LogEntryTag Color

newtype CDialogModel = CDialogModel {
  _cDistrib :: LogColorDistrib
  } deriving (Eq)

data CDialogEvent
  = ColorChange LogEntryTag Color
  deriving (Show)

makeLenses 'CDialogModel










