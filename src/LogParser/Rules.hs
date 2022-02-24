{-|
Module      : LogParser.Rules
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Parsing rules module
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module LogParser.Rules where

import Control.Lens
import Control.Applicative
import Data.Text ( pack, Text )
import qualified Data.List as L
import qualified Text.Parsec as Parsec
import Text.Parsec ( (<?>) )

import LogException
import LogParser.LogEntry


data LogParseConfig = LogParseConfig    -- empty for now
        deriving Show

pMany1 :: (Parsec.Stream Text Identity t) => Parsec.ParsecT Text u Identity Char 
    -> Parsec.ParsecT Text u Identity Text
pMany1 p = pack <$> Parsec.many1 p

pString :: (Parsec.Stream Text Identity t) => String 
    -> Parsec.ParsecT Text u Identity Text
pString s = pack <$> Parsec.string s

pGameEntityPart :: Parsec.Parsec Text LogParseConfig String
pGameEntityPart = do
    a <- Parsec.upper
    ss <- Parsec.many1 (Parsec.noneOf [' ']) -- Parsec.letter
    Parsec.spaces
    return $ a : ss

pGameEntity :: Parsec.Parsec Text LogParseConfig Text
pGameEntity  = do
    gameEntity <- Parsec.manyTill pGameEntityPart 
        (Parsec.try (Parsec.lookAhead Parsec.lower <|> Parsec.oneOf ",:."))
    return $ pack (L.unwords gameEntity)

pDorfFull :: Parsec.Parsec Text LogParseConfig Dorf
pDorfFull = do
    nicknameStartMb <- Parsec.optionMaybe (Parsec.char '`')
    nickname <- mapM (\_ -> do
            str <- Parsec.many1 (Parsec.noneOf ['\''])
            _ <- Parsec.char '\''
            Parsec.spaces 
            return $ pack str
        ) nicknameStartMb
    nameS <- pMany1 (Parsec.noneOf [',','\n'])
    Parsec.string ", "
    profS <- pGameEntity
    let dorf = Dorf nameS nickname profS
    return dorf

pDorfProf :: Parsec.Parsec Text LogParseConfig Dorf
pDorfProf = do
    profS <- pMany1 (Parsec.noneOf [' '])
    return $ Dorf "" Nothing profS

-- ****************************************************************************

-- | Parsing rules for each LogEntryTag constructor
pLogEntryData :: LogEntryTag -> Parsec.Parsec Text LogParseConfig LogEntryData
pLogEntryData LEDefault = do
    warn <- pMany1 (Parsec.noneOf ['\n','\r'])
    Parsec.spaces
    --Parsec.parserTrace "label1"
    --Parsec.endOfLine
    return $ newLogEntryData & warns .~ [warn] 

pLogEntryData t@LECraftCancel = do
    dorf <- pDorfFull
    Parsec.string "cancels "
    jobS <- pMany1 (Parsec.noneOf [':'])
    Parsec.string ": Needs "
    matS <- pMany1 (Parsec.noneOf ['.'])
    Parsec.char '.'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorf
        & job ?~ jobS
        & mat ?~ matS

pLogEntryData t@LEJobSuspensionConstructBuilding = do
    dorfA <- pDorfFull
    Parsec.string "cancels "
    jobS <- pack <$> Parsec.string "Construct Building"
    Parsec.string ": "
    warn <- pMany1 (Parsec.noneOf ['.'])
    Parsec.char '.'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & warns .~ [warn] 

pLogEntryData t@LECrimeTheft = do
    matS <- pGameEntity
    warnA <- pString "is missing from its proper place!"
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
        & warns .~ [warnA] 

pLogEntryData t@LEDFHackAutomation = do
    Parsec.string "Marked "
    numS <- pMany1 (Parsec.oneOf "0123456789")
    Parsec.string " items "
    warnA <- pack <$> Parsec.try (Parsec.string "to" <|> Parsec.string "for")
    Parsec.space
    jobS <- pMany1 (Parsec.noneOf ['\n','\r'])
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & mat ?~ numS
        & job ?~ jobS
        & warns .~ [warnA] 

pLogEntryData t@LEBattleMinorHitEventMiss1 = do
    Parsec.string "The "
    dorfA <- pDorfProf
    Parsec.space
    warnA <- pString "attacks" <|> pString "strikes at"
    Parsec.string " the "
    dorfB <- pDorfProf
    Parsec.string " but "
    warnB <- pMany1 (Parsec.noneOf ['!'])
    Parsec.char '!'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & dorf2 ?~ dorfB
        & warns .~ [warnA, warnB] 

-- | Base parsing rule; place move complex and more friquent rules to top
baseRule :: Parsec.Parsec Text LogParseConfig LogEntryData
baseRule = 
        Parsec.try (pLogEntryData LECraftCancel)
    <|> Parsec.try (pLogEntryData LEJobSuspensionConstructBuilding)
    <|> Parsec.try (pLogEntryData LECrimeTheft)
    <|> Parsec.try (pLogEntryData LEDFHackAutomation)
    <|> Parsec.try (pLogEntryData LEBattleMinorHitEventMiss1)
    <|> pLogEntryData LEDefault

parseLogEntry :: LogParseConfig -> Text -> LogEntryData
parseLogEntry lpCfg txt = 
    case Parsec.runParser baseRule lpCfg "" txt of
        (Right v)   -> v
        (Left s)    -> error $ "parse fail: "++show s

-- Used in tests
parseLogEntrySingle :: LogParseConfig 
    -> Parsec.Parsec Text LogParseConfig LogEntryData -> Text -> LogEntryData
parseLogEntrySingle lpCfg rule txt = 
    case Parsec.runParser rule lpCfg "" txt of
        (Right v)   -> v
        (Left s)    -> error $ "parse fail: "++show s

