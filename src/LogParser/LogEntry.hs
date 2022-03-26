{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LogParser.LogEntry where

import Control.Lens ( makeLenses )
import Data.Text ( Text )
import TextShow.TH ( deriveTextShow )
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Options of dorf's name display
data ShowNameType = SNFullName | SNNameOnly | SNNicknameOnly
        deriving (Show, Eq)

data LogEntryTag = 
        LEDefault 
        | LEAdoption
        | LEAnimalBirth
        | LEAnimalGrown
        | LEAnimalMisc
        | LEAnimalSlaughtered 
        | LEAnimalTraining
        | LEBattleBreath
        | LEBattleEvade
        | LEBattleEvent 
        | LEBattleEvent2
        | LEBattleHit
        | LEBattleMiss
        | LEBattleStatus
        | LEBattleStrike
        | LEBattleTrance
        | LECraftCancel 
        | LECrimeTheft
        | LEDeath
        | LEDeathFound
        | LEDFHackAutomation 
        | LEEmotion
        | LEFishing
        | LEIntruders
        | LEItem
        | LEJobCancel
        | LEJobSuspension
        | LEGhost
        | LEGore
        | LEGuild
        | LEHazard
        | LEMandate
        | LEMasterpieceCreated
        | LEMasterpieceImproved
        | LEMasterpieceLost
        | LEMiningStruck 
        | LEMiningWarning
        | LEMigrants
        | LEMoodDepression
        | LEMoodInsane
        | LEMoodNormal
        | LEMoodTantrum
        | LENecromancy
        | LEProductionCompleted
        | LESeason
        | LESettlement
        | LESkillLevel
        | LESocial
        | LESomeoneBecome
        | LESting
        | LESystem
        | LETitan
        | LETrade
        | LEVisit
        | LEWeather
        | LEWerebeast
    deriving (Show, Eq, Enum, Ord, Read, Bounded, Generic, NFData)

$(deriveTextShow ''LogEntryTag)

type Name = Text

data Actor 
        = Dorf
            { _name     :: !Name
            , _nickname :: Maybe Name
            , _prof     :: !Name
            } 
        | Creature !Name
        | Nobody
        deriving (Show, Eq, Generic, NFData)

data LogEntryData = LogEntryData
        { _tag      :: !LogEntryTag
        , _ac1      :: Maybe Actor
        , _ac2      :: Maybe Actor
        , _job      :: Maybe Name
        , _mat      :: Maybe Name
        , _strs     :: [Text]
    } deriving (Show, Eq, Generic, NFData)

makeLenses ''Actor
makeLenses ''LogEntryData

{-# INLINE newLogEntryData #-}
newLogEntryData :: LogEntryData
newLogEntryData = LogEntryData LEDefault Nothing Nothing Nothing Nothing []





