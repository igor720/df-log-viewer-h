{-|
Module      : Main
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Main application module
-}
module Main where

import System.FilePath ( takeDirectory, (</>) )
import System.Environment ( getArgs, getExecutablePath )
import System.Exit ( exitSuccess )
import Control.Exception
import qualified Data.List as List
import Data.Text ( unpack )
import Data.Maybe ( fromMaybe )

import AppException
import Config
import LogFile
import GUI
import GUI.App


mainConfigFile :: String
mainConfigFile = "dflv.yaml"

commandLineHelp :: String 
commandLineHelp = "command line: "
                ++"dfvh(.exe) [--dir=\"working directory\"] [logfilepath]"

-- | Parses working dir from a command line argument
parsePath :: FilePath -> Maybe FilePath
parsePath s = 
    if List.take 6 s=="--dir=" 
        then Just $ List.drop 6 s
        else Nothing

-- | Parses working dir and gamelog path from a command line (both are optional)
parsArgs :: [String] -> Maybe (Maybe FilePath, Maybe FilePath)
parsArgs args = case args of
    []  -> Just (Nothing, Nothing)
    [s] -> case parsePath s of
            Nothing -> Just (Nothing, Just s)
            p       -> Just (p, Nothing)
    s0:s1:_ -> case (parsePath s0, parsePath s1) of
            (Nothing, Nothing)  -> Just (Nothing, Just s0)
            (p, Nothing)        -> Just (p, Nothing)
            (Nothing, p)        -> Just (Nothing, p)
            _                   -> Nothing

main :: IO ()
main = do
    argsParsed <- parsArgs <$> getArgs
    (pathArg, logFile) <- case argsParsed of
        Just as -> return as
        Nothing -> putStrLn commandLineHelp >> exitSuccess
    pathExe <- takeDirectory <$> getExecutablePath
    let path = fromMaybe pathExe pathArg
    check <- checkMainConfig <$> readMainConfig (path </> mainConfigFile)
    cfg <- case check of
        Left msg    -> throw $ ExConfigCheck msg
        Right cfg'  -> return cfg'
    logFileName <- getLogFileName path logFile (unpack <$> _acLogFilePath cfg)
    aws <- readAppWindowSize path cfg
    gui path cfg aws logFileName






