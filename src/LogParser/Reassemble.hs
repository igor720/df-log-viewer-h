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
    np n p = if T.null n then p else n<>", "<>p
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
    f :: Lens' LogEntryData (Maybe Text) -> Text
    f l = fromMaybe 
        "<field missed>"
        (led^. l)
    d :: Lens' LogEntryData (Maybe Dorf) -> Text
    d l = makeName $ fromMaybe 
        (Dorf "<dorf missed>" Nothing "<profession missed>")
        (led^. l)
    w :: Lens' LogEntryData [Text] -> Int -> Text
    w l i = res where
        res = if T.null str
            then "<missed>"
            else str
        str = led^. l.ix i
    ss = case led^. tag of

        LEDefault -> ("default", 
            map (LEC LECOther) $ T.words (T.concat (led^.warns))
            )
        LECraftCancel -> ("craft: cancelation", concat
            [ [LEC LECJob (f job)]
            , [LEC LECOther ":"]
            , [LEC LECDorf (d dorf1)]
            , [LEC LECOther "needs"]
            , [LEC LECMat (f mat)]
            ])
        LEJobSuspensionConstructBuilding -> ("construction: suspension", concat
            [ map (LEC LECOther) $ T.words (w warns 0)
            , [LEC LECOther ":"]
            , [LEC LECDorf (d dorf1)]
            , []
            ])
        LECrimeTheft -> ("crime: theft", concat
            [ [LEC LECMat (f mat)]
            , [LEC LECOther (w warns 0)]
            , []
            ])
        LEDFHackAutomation -> ("dfhack: automation", concat
            [ [LEC LECOther "Marked "]
            , [LEC LECMat (f mat)]
            , [LEC LECMat "items"]
            , [LEC LECOther (w warns 0)]
            , [LEC LECOther (f job)]
            , []
            ])
        LEBattleMinorHitEventMiss1 -> ("battle: miss", concat
            [ [LEC LECDorf (d dorf1)]
            , map (LEC LECOther) $ T.words (w warns 0)
            , [LEC LECDorf (d dorf2)]
            , map (LEC LECOther) $ T.words (", but "<>w warns 1<>"!")
            ])

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





