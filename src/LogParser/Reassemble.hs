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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}

module LogParser.Reassemble where

import Control.Exception
import Control.Lens
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Data.Text ( Text )
import Data.Typeable ( cast )

import LogParser.LogEntry


type LogEntryDescription = Text

data LEComponentId = LECJob | LECMat | LECDorf | LECOther
        deriving (Show, Eq)
data LEComponent = LEC LEComponentId Text
        deriving (Show, Eq)
type ReLogEntry = [LEComponent]

data ReConfig = ReConfig
    { reShowProfession  :: Bool
    , reShowNameType    :: ShowNameType
    } deriving Show

reassemble :: ReConfig -> LogEntryData -> (LogEntryDescription, ReLogEntry)
reassemble reCfg led = ss where
    np n p 
        | T.null n  = p 
        | T.null p  = n 
        | otherwise = n<>", "<>p
    np1 n p = if T.null n then p else n
    nk nick = "`"<>nick<>"'"
    makeName dorf = case (reCfg, dorf) of
        (ReConfig True  SNFullName,     Dorf n Nothing p)       -> np n p
        (ReConfig True  SNFullName,     Dorf n (Just nick) p)   -> nk nick<>" "<>np n p
        (ReConfig True  SNNameOnly,     Dorf n Nothing p)       -> np n p 
        (ReConfig True  SNNameOnly,     Dorf n (Just nick) p)   -> np n p
        (ReConfig True  SNNicknameOnly, Dorf n Nothing p)       -> np n p 
        (ReConfig True  SNNicknameOnly, Dorf n (Just nick) p)   -> np nick p
        (ReConfig False SNFullName,     Dorf n Nothing p)       -> np1 n p
        (ReConfig False SNFullName,     Dorf n (Just nick) p)   -> nk nick<>" "<>n
        (ReConfig False SNNameOnly,     Dorf n Nothing p)       -> np1 n p
        (ReConfig False SNNameOnly,     Dorf n (Just nick) p)   -> np1 n p
        (ReConfig False SNNicknameOnly, Dorf n Nothing p)       -> np1 n p
        (ReConfig False SNNicknameOnly, Dorf n (Just nick) p)   -> nick
        _ -> throw $ MissedDorf led
    j :: [LEComponent]
    j = [LEC LECJob (fromMaybe "<missed job>" (led^. job))]
    m :: [LEComponent]
    m = [LEC LECMat (fromMaybe "<missed material>" (led^. mat))]
    a :: Maybe Actor -> [LEComponent]
    a ac = case ac of
            Nothing -> []
            Just a  -> case a of
                Nobody      -> []
                Creature n  -> [LEC LECDorf n]
                d@Dorf {}   -> [LEC LECDorf (makeName d)]
    a1 :: [LEComponent]
    a1 = a (led^. ac1)
    a2 :: [LEComponent]
    a2 = a (led^. ac2)
    w :: Int -> [LEComponent]
    w i = map (LEC LECOther) $ T.words (led^. strs.ix i)
    o :: Text -> [LEComponent]
    o txt = map (LEC LECOther) $ T.words txt
    o1 :: Text -> [LEComponent]
    o1 txt = [LEC LECOther txt]
    ss = case led^. tag of
        LEDefault -> ("default", w 0)
        LECraftCancel -> ("craft: cancelation1", concat 
            [ j, o1 ":", a1, o1 "needs", m ])
        LEJobSuspensionBuilding -> ("suspension: building", concat
            [ w 0, o1 ":", a1 ])
        LEJobSuspensionLinkage -> ("suspension: linkage", concat
            [ o1 "from", m ])
        LEJobSuspensionConstruction -> ("suspension: construction", m)
        LEJobCancel -> ("job: cancel", concat
            [ j, o1 ":", a1, w 0 ])
        LEProductionCompleted -> ("production: completed", concat
            [ j, o1 " (", m, o1 ") " ])
        LEMasterpieceImproved -> ("masterpiece: improved", concat
            [ m, o1 "by", a1 ])
        LEDeathFound -> ("death: found", concat
            [ a1, w 0 ])
        LECrimeTheft -> ("crime: theft", concat
            [ m, w 0 ])
        LEDFHackAutomation -> ("dfhack: automation", concat
            [ o1 "Marked ", m, o1 "items", w 0, j ])
        LEBattleMiss -> ("battle: miss", concat
            [ w 0, a1, w 1, a2, w 2 ])
        LEBattleEvent -> ("battle: event", concat
            [ w 0, a1, w 1, a2, w 3 ])
        LEBattleStrike -> ("battle: strike", concat
            [ a1, w 0, a2 ])
        LEBattleHit -> ("battle: hit", w 0)
        LEBattleEvade -> ("battle: evade", concat
            [ w 0, a1, w 1, a2, w 3 ])
        LEBattleStatus -> ("battle: status", concat
            [ a1, w 0 ])
        LEGore -> ("gore", w 0)
        LEAnimalGrown -> ("animal: grown", m)
        LEAnimalBirth -> ("animal: birth", concat
            [ m, w 0 ])
        LEWeather -> ("weather", w 0)
        LESeason -> ("season", w 0)
        LESystem -> ("system", w 0)

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





