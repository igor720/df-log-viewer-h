{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LogParser.LogEntry where

import Control.Lens ( makeLenses )
import Data.Text ( Text )
import TextShow.TH ( deriveTextShow )


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
    deriving (Show, Eq, Enum, Ord, Read, Bounded)

$(deriveTextShow ''LogEntryTag)

type Name = Text

data Actor 
        = Dorf
            { _name     :: Name
            , _nickname :: Maybe Name
            , _prof     :: Name
            } 
        | Creature Name
        | Nobody
        deriving (Show, Eq)

data LogEntryData = LogEntryData
        { _tag      :: LogEntryTag
        , _ac1      :: Maybe Actor
        , _ac2      :: Maybe Actor
        , _job      :: Maybe Name
        , _mat      :: Maybe Name
        , _strs     :: [Text]
    } deriving (Show, Eq)

makeLenses ''Actor
makeLenses ''LogEntryData

{-# INLINE newLogEntryData #-}
newLogEntryData :: LogEntryData
newLogEntryData = LogEntryData LEDefault Nothing Nothing Nothing Nothing []





