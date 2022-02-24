module Main where

import Test.HUnit
import Test.LogFile
--import System.IO
import System.Exit
-- import System.Exit.Codes
import Control.Monad
import Test.Rules

tests :: Test
tests = TestList 
    [ TestLabel "latestLogCut" tlatestLogCut
    , TestLabel "getLogFileName" tgetLogFileName
    , TestLabel "pLogEntryData" tpLogEntryData
    ]

-- Due to inconsistency of some dependencies for testing packages 
-- and snapshot's packages on Windows, 
-- we do only basic testing with Test.HUnit for now.
main :: IO ()
main = do
    counts <- runTestTT tests
    when (errors counts>0 || failures counts>0) 
        exitFailure





