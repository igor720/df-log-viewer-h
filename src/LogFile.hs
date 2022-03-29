{-|
Module      : LogFile
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Gamelog file reading module
-}
{-# LANGUAGE OverloadedStrings #-}

module LogFile where

import System.Directory
import System.FilePath
import Control.Monad ( filterM )
import Control.Exception ( bracket )
import qualified Data.List as List
import Data.Text ( Text )
import Data.Maybe ( fromJust, fromMaybe, isJust )

{-# INLINE sizeOfBlock #-}
{-# INLINE readTimeout #-}
{-# INLINE gamelogFileName #-}

type LogBlocks = [[Text]]

sizeOfBlock :: Int
sizeOfBlock = 100

readTimeout :: Int
readTimeout = 500000

gamelogFileName :: FilePath
gamelogFileName = "gamelog.txt"

-- | Get blocks of text lines of last saved log entries
latestLogCut :: Int -> [Text] -> LogBlocks
latestLogCut numberOfEntries ls
    | numberOfEntries<0     = error "invalid numberOfEntries"
    | numberOfEntries==0    = []
    | otherwise             = resLogBlocks where
        numOfBlocks = (numberOfEntries-1) `div` sizeOfBlock + 1
        (acmLogBlocks, _, _) = List.foldl' (\(logBlocks, blockI, lineI) line ->
            let res@(logLines', blockI', lineI') = case logBlocks of
                    [] -> ([[line]], 1, 2)
                    bl':bls' -> if lineI<=sizeOfBlock
                        then ((line:bl'):bls', blockI, lineI+1)
                        else ([line]:logBlocks, blockI+1, 2)
            in if blockI'>numOfBlocks+1
                    then (init logLines', numOfBlocks+1, lineI')
                    else res
            ) ([], 1, 1) ls
        accumF rest bl
            | rest<=0   = (0, [])
            | otherwise =
                let blSize = length bl  -- can be excluded if zip acmLogBlocks with block position
                in if rest>=blSize then (rest-blSize, bl) else (0, take rest bl)
        resLogBlocks = reverse $ map reverse $ filter (not.null) $ snd $ 
            List.mapAccumL accumF numberOfEntries acmLogBlocks

-- | Concatenate all log blocks
mergeLogBlocks :: LogBlocks -> [Text]
mergeLogBlocks = concat

-- | Returns one of possible gamelog file pathes
getLogFileName :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO FilePath
getLogFileName _ (Just f) _ = do
    exists <- doesFileExist f
    if exists then return f
    else error "Gamelog file (in command line) does not exist"
getLogFileName path _ fCfg = do
    let possiblePath parentDir = do
            parentExists <- doesPathExist parentDir
            if parentExists then bracket
                getCurrentDirectory
                setCurrentDirectory
                (\_ -> do 
                    allContent <- getDirectoryContents parentDir
                    setCurrentDirectory parentDir
                    dirs <- filterM doesDirectoryExist allContent
                    let gameDirs = filter 
                            (\d -> take 14 d=="Dwarf Fortress") dirs
                    if null gameDirs then return Nothing
                    else do
                        let gameDirSelected = head gameDirs -- !! first avalable
                        exists <- doesFileExist 
                            (gameDirSelected </> gamelogFileName)
                        if not exists then return Nothing 
                        else return $ Just (parentDir </> gameDirSelected)
                )
            else return Nothing
    case fCfg of
        Just f  -> do
            exists <- doesFileExist f
            if exists then return f
            else error "Gamelog file (in config) does not exist"
        Nothing -> do
            path0 <- possiblePath $ path </> "../../../" -- LNP
            path1 <- possiblePath $ path </> "../"       -- neighbour
            let possiblePaths = [path0, path1]
                parentDir = fromJust $ fromMaybe 
                    (error "Cannot find any DF log file")
                    (List.find isJust possiblePaths)
            return $ parentDir </> gamelogFileName



