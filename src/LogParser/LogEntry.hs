{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LogParser.LogEntry where

import Control.Lens ( makeLenses )
import Data.Text ( Text )
import TextShow.TH ( deriveTextShow )
--import qualified Text.Parsec as Parsec

--import LogException


-- | Options of dorf's name display
data ShowNameType = SNFullName | SNNameOnly | SNNicknameOnly
        deriving (Show, Eq)

data LogEntryTag = 
        LEDefault 
        | LECraftCancel 
        | LEJobSuspensionBuilding | LEJobSuspensionLinkage | LEJobSuspensionConstruction 
        | LEJobCancel
        | LEProductionCompleted
        | LEMasterpieceImproved
        | LEDeathFound
        | LECrimeTheft
        | LEDFHackAutomation 
        | LEBattleBlock | LEBattleMiss
        | LEBattleEvent1 | LEBattleEvent2
        | LEBattleStrike
        | LEGore
        | LEBattleHit
        | LEBattleStatus
        | LEAnimalGrown
        | LEAnimalBirth
        | LEWeather
        | LESeason
        | LESystem
    deriving (Show, Eq, Enum, Ord, Read, Bounded)

$(deriveTextShow ''LogEntryTag)

data Dorf = Dorf
        { _name     :: Text
        , _nickname :: Maybe Text
        , _prof     :: Text
    } deriving (Show, Eq)

data LogEntryData = LogEntryData
        { _tag      :: LogEntryTag
        , _dorf1    :: Maybe Dorf
        , _dorf2    :: Maybe Dorf
        , _job      :: Maybe Text
        , _mat      :: Maybe Text
        , _warns    :: [Text]
    } deriving (Show, Eq)

makeLenses ''Dorf
makeLenses ''LogEntryData

{-# INLINE newLogEntryData #-}
newLogEntryData :: LogEntryData
newLogEntryData = LogEntryData LEDefault Nothing Nothing Nothing Nothing []

{-# INLINE missedDorf #-}
missedDorf :: Dorf
missedDorf = Dorf "<missed>" Nothing "<dorf>"




