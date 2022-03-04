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
        | LEBattleEvent 
        | LEBattleStrike
        | LEGore
        | LEBattleHit
        | LEBattleEvade
        | LEBattleStatus
        | LEAnimalGrown
        | LEAnimalBirth
        | LEWeather
        | LESeason
        | LESystem
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

-- {-# INLINE missedDorf #-}
-- missedDorf :: Dorf
-- missedDorf = Dorf "<missed>" Nothing "<dorf>"




