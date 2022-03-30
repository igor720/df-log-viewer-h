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
import System.Exit ( exitFailure )
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
                ++"dfvh(.exe) [--work-dir=\"working directory\"] "
                ++"[--gamelog=\"logfilepath\"]"

data PathOpt = WorkingDir FilePath | GameLogPath FilePath | InvalidOpt
        deriving Show

-- | Parses working dir from a command line argument
parsePath :: FilePath -> PathOpt
parsePath s
    | List.take 11 s=="--work-dir=" = WorkingDir $ List.drop 11 s
    | List.take 10 s=="--gamelog="  = GameLogPath $ List.drop 10 s
    | otherwise                     = InvalidOpt

-- | Parses working dir and gamelog path from a command line (both are optional)
parsArgs :: [String] -> Maybe (Maybe FilePath, Maybe FilePath)
parsArgs args = case args of
    []  -> Just (Nothing, Nothing)
    [s] -> case parsePath s of
            WorkingDir p    -> Just (Just p, Nothing)
            GameLogPath p   -> Just (Nothing, Just p)
            InvalidOpt     -> Nothing
    s0:s1:_ -> case (parsePath s0, parsePath s1) of
            (WorkingDir p, InvalidOpt)      -> Just (Just p, Nothing)
            (InvalidOpt, WorkingDir p)      -> Just (Just p, Nothing)
            (GameLogPath p, InvalidOpt)     -> Just (Nothing, Just p)
            (InvalidOpt, GameLogPath p)     -> Just (Nothing, Just p)
            (WorkingDir p1, GameLogPath p2) -> Just (Just p1, Just p2)
            (GameLogPath p2, WorkingDir p1) -> Just (Just p2, Just p1)
            _                               -> Nothing

main :: IO ()
main = do
    argsParsed <- parsArgs <$> getArgs
    (pathArgMb, logFileMb) <- maybe exitFailure return argsParsed
    pathExe <- takeDirectory <$> getExecutablePath
    let path = fromMaybe pathExe pathArgMb
    check <- checkMainConfig <$> readMainConfig (path </> mainConfigFile)
    cfg <- case check of
        Left msg    -> throw $ ExConfigCheck msg
        Right cfg'  -> return cfg'
    logFileName <- getLogFileName path logFileMb (unpack <$> _acLogFilePath cfg)
    aws <- readAppWindowSize path cfg
    gui path cfg aws logFileName





