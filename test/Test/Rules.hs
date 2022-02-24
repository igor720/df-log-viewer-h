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
    , let tag=LEBattleMinorHitEventMiss1 in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The hammerdwarf strikes at the hammerdwarf but the shot is parried by the (bismuth bronze battle axe)!" ~?= 
        LogEntryData tag 
            (Just (Dorf {_name = "", _nickname = Nothing, _prof = "hammerdwarf"}))
            (Just (Dorf {_name = "", _nickname = Nothing, _prof = "hammerdwarf"}))
            Nothing
            Nothing
            ["strikes at","the shot is parried by the (bismuth bronze battle axe)"]


    ] where
        cfg = LogParseConfig
