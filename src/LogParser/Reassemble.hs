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
        --(_                        ,     Dorf "" Nothing "")     -> ""
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
        _ -> error "" --throw MissedDorf
    f :: Lens' LogEntryData (Maybe Text) -> Text
    f l = fromMaybe 
        "<field missed>"
        (led^. l)
    a :: Lens' LogEntryData (Maybe Actor) -> Text
    a l = case led^. l of
            Nothing -> ""
            Just a -> case a of
                Nobody -> ""
                Creature n -> n
                d@Dorf {} -> makeName d
    w :: Lens' LogEntryData [Text] -> Int -> Text
    w l i = res where
        res = if T.null str
            then ""
            else str
        str = led^. l.ix i
    isNoDorf "" = True
    isNoDorf _ = False
    ss = case led^. tag of
        LEDefault -> ("default", 
            map (LEC LECOther) $ T.words (T.concat (led^.strs))
            )
        LECraftCancel -> ("craft: cancelation1", concat
            [ [LEC LECJob (f job)]
            , [LEC LECOther ":"]
            , [LEC LECDorf (a ac1)]
            , [LEC LECOther "needs"]
            , [LEC LECMat (f mat)]
            ])
        LEJobSuspensionBuilding -> ("suspension: building", concat
            [ map (LEC LECOther) $ T.words (w strs 0)
            , [LEC LECOther ":"]
            , [LEC LECDorf (a ac1)]
            ])
        LEJobSuspensionLinkage -> ("suspension: linkage", concat
            [ map (LEC LECOther) ["from"]
            , [LEC LECMat (f mat)]
            ])
        LEJobSuspensionConstruction -> ("suspension: construction",
            [ LEC LECMat (f mat)]
            )
        LEJobCancel -> ("job: cancel", concat
            [ [LEC LECJob (f job)]
            , [LEC LECOther ":"]
            , [LEC LECDorf (a ac1)]
            , [LEC LECOther (w strs 0)]
            ])
        LEProductionCompleted -> ("production: completed", concat
            [ [LEC LECJob (f job)]
            , [LEC LECOther " ("]
            , [LEC LECMat (f mat)]
            , [LEC LECOther ") "]
            ])
        LEMasterpieceImproved -> ("masterpiece: improved", concat
            [ [LEC LECMat (f mat)]
            , [LEC LECOther "by"]
            , [LEC LECDorf (a ac1)]
            ])
        LEDeathFound -> ("death: found", concat
            [ [LEC LECDorf (a ac1)]
            , [LEC LECOther (w strs 0)]
            ])
        LECrimeTheft -> ("crime: theft", concat
            [ [LEC LECMat (f mat)]
            , [LEC LECOther (w strs 0)]
            , []
            ])
        LEDFHackAutomation -> ("dfhack: automation", concat
            [ [LEC LECOther "Marked "]
            , [LEC LECMat (f mat)]
            , [LEC LECMat "items"]
            , [LEC LECOther (w strs 0)]
            , [LEC LECOther (f job)]
            , []
            ])
        LEBattleBlock -> ("battle: block", concat
            [ [LEC LECDorf (a ac1)]
            , map (LEC LECOther) [w strs 0]
            , [LEC LECDorf (a ac2)]
            , map (LEC LECOther) $ T.words (", "<>w strs 1<>"!")
            ])
        LEBattleMiss -> ("battle: miss", concat
            [ [LEC LECDorf (a ac1)]
            , map (LEC LECOther) [w strs 0]
            , [LEC LECDorf (a ac2)]
            ])
        LEBattleEvent1 -> ("battle: event1", concat
            [ [LEC LECDorf (a ac1)]
            , map (LEC LECOther) [w strs 0]
            , [LEC LECDorf (a ac2)]
            ])
        LEBattleEvent2 -> ("battle: event2", concat
            [ [LEC LECDorf (a ac1)]
            , map (LEC LECOther) [w strs 0]
            ])
        LEBattleStrike -> ("battle: strike", concat
            [ [LEC LECDorf (a ac1)]
            , map (LEC LECOther) [w strs 0]
            , [LEC LECDorf (a ac2)]
            ])
        LEBattleHit -> ("battle: hit", map (LEC LECOther) [w strs 0])
        LEBattleStatus -> ("battle: status", concat $
            ( if isNoDorf (a ac1) then [LEC LECOther ""] else [LEC LECDorf (a ac1)])
            : [ map (LEC LECOther) [w strs 0] ]
            )
        LEGore -> ("gore", map (LEC LECOther) [w strs 0])
        LEAnimalGrown -> ("animal: grown", 
            [LEC LECMat (f mat)]
            )
        LEAnimalBirth -> ("animal: birth", concat
            [ [LEC LECMat (f mat)]
            , map (LEC LECOther) [w strs 0]
            ])
        LEWeather -> ("weather", map (LEC LECOther) [w strs 0])
        LESeason -> ("season", map (LEC LECOther) [w strs 0])
        LESystem -> ("system", map (LEC LECOther) [w strs 0])

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
    deriving Show

instance Exception MissedDorf where
    toException   = logEntryExceptionToException
    fromException = logEntryExceptionFromException

data MissedWarn = MissedWarn LogEntryData Int
    deriving Show

instance Exception MissedWarn where
    toException   = logEntryExceptionToException
    fromException = logEntryExceptionFromException





