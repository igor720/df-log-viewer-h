{-|
Module      : LogParser.Reassemble
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Reassamble log text from components
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}

module LogParser.Reassemble where

import Control.Exception
import Control.Lens
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Data.Text ( Text )
import Data.Typeable ( cast )
import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import LogParser.LogEntry


type LogEntryDescription = Text

data LEComponentId = LECJob | LECMat | LECDorf | LECOther
        deriving (Show, Eq, Generic, NFData)
data LEComponent = LEC !LEComponentId !Text 
        deriving (Show, Eq, Generic, NFData)

data ReLogEntry = ReLogEntry
        { _rleTag       :: !LogEntryTag 
        , _rleDesc      :: !LogEntryDescription 
        , _rleComps     :: [LEComponent]
        } deriving (Show, Eq, Generic, NFData)

makeLenses ''ReLogEntry

data ReConfig = ReConfig
    { reShowProfession  :: Bool
    , reShowNameType    :: ShowNameType
    } deriving Show

reassemble :: ReConfig -> LogEntryData -> ReLogEntry
reassemble reCfg led = ss where
    np n p 
        | T.null p  = n 
        | otherwise = n<>", "<>p
    --np1 n p = if T.null n then p else n
    nk nick = "`"<>nick<>"'"
    makeName dorf = case (reCfg, dorf) of
        (ReConfig True  SNFullName,     Dorf n Nothing p)       -> np n p
        (ReConfig True  SNFullName,     Dorf n (Just nick) p)   -> nk nick<>" "<>np n p
        (ReConfig True  SNNameOnly,     Dorf n Nothing p)       -> np n p 
        (ReConfig True  SNNameOnly,     Dorf n (Just _) p)      -> np n p
        (ReConfig True  SNNicknameOnly, Dorf n Nothing p)       -> np n p 
        (ReConfig True  SNNicknameOnly, Dorf _ (Just nick) p)   -> np nick p
        (ReConfig False SNFullName,     Dorf n Nothing _)       -> n
        (ReConfig False SNFullName,     Dorf n (Just nick) _)   -> nk nick<>" "<>n
        (ReConfig False SNNameOnly,     Dorf n Nothing _)       -> n
        (ReConfig False SNNameOnly,     Dorf n (Just _) _)      -> n
        (ReConfig False SNNicknameOnly, Dorf n Nothing _)       -> n
        (ReConfig False SNNicknameOnly, Dorf _ (Just nick) _)   -> nick
        _ -> throw $ MissedDorf led
    j :: [LEComponent]
    j = [LEC LECJob (fromMaybe "" (led^. job))]
    m :: [LEComponent]
    m = [LEC LECMat (fromMaybe "" (led^. mat))]
    a :: Maybe Actor -> [LEComponent]
    a ac = case ac of
            Nothing -> []
            Just actor  -> case actor of
                Nobody      -> []
                Creature n  -> [LEC LECDorf n]
                d@Dorf {}   -> [LEC LECDorf (makeName d)]
    a1 :: [LEComponent]
    a1 = a (led^. ac1)
    a2 :: [LEComponent]
    a2 = a (led^. ac2)
    w :: Int -> [LEComponent]
    w i = map (LEC LECOther) $ T.words (led^. strs.ix i)
    --o :: Text -> [LEComponent]
    --o txt = map (LEC LECOther) $ T.words txt
    o1 :: Text -> [LEComponent]
    o1 txt = [LEC LECOther txt]
    c desc comps = ReLogEntry (led^. tag) desc (concat comps)
    ss = case led^. tag of
        LEDefault ->            c "default"                 [w 0]
        LEJobSuspension ->      c "job: suspension"         [ w 0, j, w 1, m, w 2, a1, o1 "." ]
        LECraftCancel ->        c "craft: cancel"           [ j, o1 ":", a1, o1 "needs", m ]
        LEJobCancel ->          c "job: cancel"             [ a1, w 0, j, w 1, w 2 ]
        LEProductionCompleted -> c "production: completed"  [ j, o1 " (", m, o1 ") " ]
        LEMasterpieceImproved -> c "masterpiece: improved"  [ a1, w 0, m, w 1 ]
        LEMasterpieceCreated -> c "masterpiece: created"    [ a1, w 0 ]
        LECrimeTheft ->         c "crime: theft"            [ m, w 0 ]
        LEDFHackAutomation ->   c "dfhack: automation"      [ o1 "Marked ", m, o1 "items", w 0, j ]
        LEMiningStruck ->       c "mining: struck"          [ w 0, m, o1 "!" ]
        LEBattleMiss ->         c "battle: miss"            [ w 0, a1, w 1, a2, w 2 ]
        LEBattleEvent ->        c "battle: event"           [ w 0, a1, w 1, a2, w 2 ]
        LEBattleStrike ->       c "battle: strike"          [ a1, w 0, a2 ]
        LEBattleHit ->          c "battle: hit"             [w 0]                 
        LEBattleEvade ->        c "battle: evade"           [ w 0, a1, w 1, a2, w 2 ]
        LEBattleStatus ->       c "battle: status"          [ a1, w 0 ]
        LEBattleEvent2 ->       c "battle: event2"          [ w 0, a1, w 1, a2, w 2 ]
        LEBattleTrance ->       c "battle: trance"          [ w 0, a1, w 1 ]
        LEEmotion ->            c "emotion"                 [ a1, w 0 ]
        LEGore ->               c "gore"                    [w 0]
        LEAnimalGrown ->        c "animal: grown"           [m]
        LEAnimalBirth ->        c "animal: birth"           [ m, w 0 ]
        LEAnimalSlaughtered ->  c "animal: slaughtered"     [m]
        LEAnimalTraining ->     c "animal: training"        [ w 0, a1, w 1 ]
        LEAnimalMisc ->         c "animal: misc"            [ w 0, a1, w 1 ]
        LESocial ->             c "social"                  [ w 0, a1, w 1, a2, w 2 ]
        LESomeoneBecome ->      c "someone: become"         [ a1, w 0, w 1, m ]
        LEMandate ->            c "mandate"                 [ a1, w 0 ]
        LETrade ->              c "trade"                   [ a1, w 0 ]
        LEVisit ->              c "visit"                   [ w 0, a1, w 1 ]
        LESting ->              c "sting"                   [ a1, w 0 ]
        LEItem ->               c "item"                    [ a1, w 0, m ]
        LEWeather ->            c "weather"                 [w 0]
        LEFishing ->            c "fishing"                 [w 0]
        LEAdoption ->           c "adotion"                 [ a1, w 0, a2, o1 "." ]
        LESkillLevel ->         c "skill: level"            [ a1, w 0 ]
        LEMoodNormal ->         c "mood: normal"            [ a1, w 0, m, w 1 ]
        LEMoodInsane ->         c "mood: insane"            [ a1, w 0 ]
        LEMoodTantrum ->        c "mood: tantrum"           [ a1, w 0, j, w 1 ]
        LEMoodDepression ->     c "mood: depression"        [ a1, w 0 ]
        LEGuild ->              c "guild"                   [w 0]
        LEBattleBreath ->       c "battle: breath"          [ w 0, a1, w 1 ]
        LEMasterpieceLost ->    c "masterpiece: lost"       [w 0]
        LEHazard ->             c "hazard"                  [ a1, w 0 ]
        LEMiningWarning ->      c "mining: warning"         [w 0]
        LEMigrants ->           c "migrants"                [w 0]
        LESettlement ->         c "settlement"              [w 0]
        LEDeath ->              c "death"                   [ a1, w 0 ]
        LEDeathFound ->         c "death: found"            [ a1, w 0 ]
        LENecromancy ->         c "necromancy"              [ a1, w 0 ]
        LEIntruders ->          c "intruders"               [ m, w 0, a1 ]
        LEGhost ->              c "ghost"                   [ w 0, m, w 1, a1, w 2 ]
        LEWerebeast ->          c "werebeast"               [ a1, w 0 ]
        LETitan ->              c "titan"                   [ w 0, a1, w 1 ]
        LESeason ->             c "season"                  [w 0]
        LESystem ->             c "system"                  [w 0]

-- *****************************************************************************

data LogEntryException = forall e . Exception e => LogEntryException e

instance Show LogEntryException where
    show (LogEntryException e) = show e

instance Exception LogEntryException

logEntryExceptionToException :: Exception e => e -> SomeException
logEntryExceptionToException = toException . LogEntryException

logEntryExceptionFromException :: Exception e => SomeException -> Maybe e
logEntryExceptionFromException x = do
    LogEntryException a <- fromException x
    cast a

newtype MissedField = MissedField LogEntryData
    deriving Show

instance Exception MissedField where
    toException   = logEntryExceptionToException
    fromException = logEntryExceptionFromException

newtype MissedDorf = MissedDorf LogEntryData

instance Show MissedDorf where
    show (MissedDorf led) = "reassemble fail: "++show led

instance Exception MissedDorf where
    toException   = logEntryExceptionToException
    fromException = logEntryExceptionFromException

data MissedWarn = MissedWarn LogEntryData Int
    deriving Show

instance Exception MissedWarn where
    toException   = logEntryExceptionToException
    fromException = logEntryExceptionFromException





