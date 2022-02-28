{-# LANGUAGE OverloadedStrings #-}

module Test.Rules where

import Test.HUnit
import System.FilePath
import Data.Text (Text)
import TextShow (TextShow(showt))

import LogParser.Rules
import LogParser.LogEntry


tpLogEntryData :: Test
tpLogEntryData = TestList
    [ let tag=LEDefault in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "default rule parsing" ~?= 
            LogEntryData tag 
                Nothing 
                Nothing 
                Nothing 
                Nothing 
                ["default rule parsing"]
    , let tag=LECraftCancel in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Gem Setter' Idstorlut, Gem Setter cancels Encrust Finished Goods With resin opal: Needs resin opals." ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "Idstorlut", _nickname = Just "Gem Setter", _prof = "Gem Setter"}))
            Nothing 
            (Just "Encrust Finished Goods With resin opal")
            (Just "resin opals")
            []
    , let tag=LEJobSuspensionConstructBuilding in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Doctor' Ducimmuzish, Suturer cancels Construct Building: Building site submerged." ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "Ducimmuzish", _nickname = Just "Doctor", _prof = "Suturer"}))
            Nothing
            (Just "Construct Building")
            Nothing
            ["Building site submerged"]
    , let tag=LECrimeTheft in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Zaledenseb Ilb�d Bukith is missing from its proper place!" ~?= 
        LogEntryData tag 
            Nothing
            Nothing
            Nothing
            (Just "Zaledenseb Ilb�d Bukith")
            ["is missing from its proper place!"]
    , let tag=LEDFHackAutomation in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Marked 47 items to melt" ~?= 
        LogEntryData tag 
            Nothing
            Nothing
            (Just "melt")
            (Just "47")
            ["to"]
    , let tag=LEBattleMiss1 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The hammerdwarf strikes at the hammerdwarf but the shot is parried by the (bismuth bronze battle axe)!" ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "hammerdwarf", _nickname = Nothing, _prof = ""}))
            (Just (Dorf {_name = "hammerdwarf", _nickname = Nothing, _prof = ""}))
            Nothing
            Nothing
            ["strikes at","but the shot is parried by the (bismuth bronze battle axe)"]
    , let tag=LEBattleMiss2 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The axedwarf misses the hammerdwarf!" ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "axedwarf", _nickname = Nothing, _prof = ""}))
            (Just (Dorf {_name = "hammerdwarf", _nickname = Nothing, _prof = ""}))
            Nothing
            Nothing
            ["misses"]
    , let tag=LEBattleEvent1 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The stray war dog charges at the weasel!" ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "stray war dog", _nickname = Nothing, _prof = ""}))
            (Just (Dorf {_name = "weasel", _nickname = Nothing, _prof = ""}))
            Nothing
            Nothing
            ["charges at"]
    , let tag=LEBattleEvent2 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The hammerdwarf is knocked over!" ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "hammerdwarf", _nickname = Nothing, _prof = ""}))
            Nothing
            Nothing
            Nothing
            ["is knocked over"]
    , let tag=LEBattleStrike in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The macedwarf bashes the axedwarf in the left upper arm with her (+®copper maceЇ+), lightly tapping the target!" ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "macedwarf", _nickname = Nothing, _prof = ""}))
            (Just (Dorf {_name = "axedwarf", _nickname = Nothing, _prof = ""}))
            Nothing
            Nothing
            ["bashes"," in the left upper arm with her (+\174copper mace\1031+), lightly tapping the target!"]
    , let tag=LESystem1 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Loaded region1, Xemomon (The Mythical Realms) at coordinates (113,93)" ~?= 
        LogEntryData tag 
            Nothing
            Nothing
            Nothing
            Nothing
            ["Loaded region1, Xemomon (The Mythical Realms) at coordinates (113,93)"]
    , let tag=LESystem2 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "*** STARTING NEW GAME ***" ~?= 
        LogEntryData tag 
            Nothing
            Nothing
            Nothing
            Nothing
            ["*** STARTING NEW GAME ***"]



    ] where
        cfg = LogParseConfig
