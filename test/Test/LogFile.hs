{-# LANGUAGE OverloadedStrings #-}

module Test.LogFile where

import Test.HUnit
import System.FilePath
import Data.Text (Text)
import TextShow (TextShow(showt))

import LogFile


tlatestLogCut :: Test
tlatestLogCut = TestList
    [ "small data" ~: 
        [ "cut 0" ~: latestLogCut 0 data0 ~?= []
        , "cut 1" ~: latestLogCut 1 data0 ~?= [["c"]]
        , "cut 2" ~: latestLogCut 2 data0 ~?= [["b", "c"]]
        , "cut 3" ~: latestLogCut 3 data0 ~?= [data0]
        , "cut 4" ~: latestLogCut 4 data0 ~?= [data0]
        ]
    , "big data 1" ~: 
        [ "cut 0" ~: latestLogCut 0 data1 ~?= []
        , "cut 2" ~: latestLogCut 2 data1 ~?= [["319", "320"]]
        , "cut 300" ~: latestLogCut 300 data1 ~?= data1res'
        , "cut 319" ~: latestLogCut 315 data1 ~?= data1res''
        , "cut 320" ~: latestLogCut 320 data1 ~?= data1res
        , "cut 400" ~: latestLogCut 400 data1 ~?= data1res
        ]
    , "big data 2" ~: 
        [ "cut 0" ~: latestLogCut 0 data2 ~?= []
        , "cut 2" ~: latestLogCut 2 data2 ~?= [["299", "300"]]
        , "cut 200" ~: latestLogCut 200 data2 ~?= data2res'
        , "cut 199" ~: latestLogCut 199 data2 ~?= data2res''
        , "cut 201" ~: latestLogCut 201 data2 ~?= data2res'''
        , "cut 300" ~: latestLogCut 300 data2 ~?= data2res
        ]
    ] where
        data0 = ["a", "b", "c"]
        gen :: Int -> Int -> [Int]
        gen n a = take n $ iterate (+1) a
        tList :: Int -> Int -> [Text]
        tList n a = map showt $ gen n a
        data1 = tList 320 1
        data1res = [tList 100 1, tList 100 101, tList 100 201, tList 20 301]
        data1res' = [tList 80 21, tList 100 101, tList 100 201, tList 20 301]
        data1res'' = [tList 95 6, tList 100 101, tList 100 201, tList 20 301]
        data2 = tList 300 1
        data2res' = [tList 100 101, tList 100 201]
        data2res'' = [tList 99 102, tList 100 201]
        data2res''' = [tList 1 100, tList 100 101, tList 100 201]
        data2res = [tList 100 1, tList 100 101, tList 100 201]

tgetLogFileName :: Test
tgetLogFileName = TestList
    [ "command line argument" ~: do
        let filename' = "./test/data/gamelog/Dwarf Fortress x"</>gamelogFileName
        filename <- getLogFileName "" (Just filename') Nothing
        filename @?= filename'
    , "config parameter" ~: do
        let filename' = "./test/data/gamelog/Dwarf Fortress x"</>gamelogFileName
        filename <- getLogFileName "" Nothing (Just filename')
        filename @?= filename'
    , "LNP path" ~: do
        let filename' = exePath0</>"../../../Dwarf Fortress x"</>gamelogFileName
        filename <- getLogFileName exePath0 Nothing Nothing
        filename @?= filename'
    , "neighbour directory" ~: do
        let filename' = exePath1</>"../Dwarf Fortress x"</>gamelogFileName
        filename <- getLogFileName exePath1 Nothing Nothing
        filename @?= filename'
    ] where
        exePath0 = "./test/data/gamelog/LNP/utilites/exepath0"
        exePath1 = "./test/data/gamelog/exepath1"




