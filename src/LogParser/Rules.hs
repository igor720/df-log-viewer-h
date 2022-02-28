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
import Data.Text ( pack, unpack, Text )
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

pSomeone :: [String] -> Parsec.Parsec Text LogParseConfig Text
pSomeone endWith = do
    str <- Parsec.lookAhead (Parsec.many (Parsec.noneOf ['\n','\r']))
    let ws = L.takeWhile (`notElem` endWith) $ L.words str
        someone = L.unwords ws
    Parsec.string someone
    return $ pack someone

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

pLogEntryData t@LEBattleMiss1 = do
    Parsec.string "The "
    someoneA <- pSomeone ["attacks", "strikes"]
    Parsec.space
    warnA <- pString "attacks" <|> pString "strikes at"
    Parsec.string " the "
    someoneB <- pSomeone ["but"]
    Parsec.space
    warnB <- pMany1 (Parsec.noneOf ['!'])
    Parsec.char '!'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA, warnB] 

pLogEntryData t@LEBattleMiss2 = do
    Parsec.string "The "
    someoneA <- pSomeone ["misses"]
    Parsec.space
    warnA <- pString "misses"
    Parsec.string " the "
    someoneB <- pMany1 (Parsec.noneOf ['!'])
    Parsec.char '!'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA] 

pLogEntryData t@LEBattleEvent1 = do
    Parsec.string "The "
    someoneA <- pSomeone ["charges", "collides"]
    Parsec.space
    warnA' <- Parsec.char 'c'
    warnA''<- Parsec.string "harges at" <|> Parsec.string "ollides with"
    let warnA = pack $ warnA':warnA''
    Parsec.string " the "
    someoneB <- pMany1 (Parsec.noneOf ['!'])
    Parsec.char '!'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA]

pLogEntryData t@LEBattleEvent2 = do
    Parsec.string "The "
    someoneA <- pSomeone ["has", "is", "stands", "passes", "falls", "regains"]
    Parsec.space
    warnA <- Parsec.try (pString "has been stunned")            <|> Parsec.try (pString "is knocked over")
        <|> Parsec.try (pString "has been knocked unconscious") <|> Parsec.try (pString "stands up")  
        <|> Parsec.try (pString "passes out")                   <|> Parsec.try (pString "falls over")
        <|> Parsec.try (pString "regains consciousness")        <|> pString "is no longer stunned"
    Parsec.char '!' <|> Parsec.char '.'
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & warns .~ [warnA]

pLogEntryData t@LEBattleStrike = do
    Parsec.string "The "
    -- Parsec.parserTrace "label1"
    someoneA <- pSomeone 
        ["leaps","punches","punches","catches","snatches","stabs"
        ,"grabs","hacks","pushes","misses","slashes","shakes"
        ,"blocks","gores","strangles","strikes","scratches","kicks"
        ,"attacks","lashes","slaps","bashes","bites","strikes"
        ,"punches","releases","throws","takes","locks","bends"
        ,"places","gouges"]
    Parsec.space
    -- Parsec.parserTrace "label2"
    warnA <- Parsec.try (pString "leaps at")<|> Parsec.try (pString "punches")  <|> Parsec.try (pString "punches")
        <|> Parsec.try (pString "catches")  <|> Parsec.try (pString "snatches at") <|> Parsec.try (pString "stabs")
        <|> Parsec.try (pString "grabs")    <|> Parsec.try (pString "hacks")    <|> Parsec.try (pString "pushes")
        <|> Parsec.try (pString "misses")   <|> Parsec.try (pString "slashes")  <|> Parsec.try (pString "shakes")
        <|> Parsec.try (pString "blocks")   <|> Parsec.try (pString "gores")    <|> Parsec.try (pString "strangles")
        <|> Parsec.try (pString "strikes")  <|> Parsec.try (pString "scratches")<|> Parsec.try (pString "kicks")
        <|> Parsec.try (pString "attacks")  <|> Parsec.try (pString "lashes")   <|> Parsec.try (pString "slaps")
        <|> Parsec.try (pString "bashes")   <|> Parsec.try (pString "bites")    <|> Parsec.try (pString "strikes at")
        <|> Parsec.try (pString "punches")  <|> Parsec.try (pString "releases") <|> Parsec.try (pString "throws")
        <|> Parsec.try (pString "takes")    <|> Parsec.try (pString "locks")    <|> Parsec.try (pString "bends")
        <|> Parsec.try (pString "places a chokehold on")                        <|> pString "gouges"
    -- Parsec.parserTrace "label3"
    Parsec.string " the "
    someoneB <- pSomeone ["in"]
    warnB <- pMany1 (Parsec.noneOf ['\n','\r'])
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA, warnB]

pLogEntryData t@LESystem1 = do
    warnA' <- pString "Loaded "
    warnA'' <- pMany1 (Parsec.noneOf ['\r', '\n'])
    let warnA = warnA'<>warnA''
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]

pLogEntryData t@LESystem2 = do
    warnA' <- Parsec.char '*'
    warnA'' <- Parsec.many1 (Parsec.noneOf ['\r', '\n'])
    let warnA = pack $ warnA':warnA''
    Parsec.spaces 
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]


-- | Base parsing rule; place move specific and more friquent rules to top
baseRule :: Parsec.Parsec Text LogParseConfig LogEntryData
baseRule = 
        Parsec.try (pLogEntryData LECraftCancel)
    <|> Parsec.try (pLogEntryData LEJobSuspensionConstructBuilding)
    <|> Parsec.try (pLogEntryData LECrimeTheft)
    <|> Parsec.try (pLogEntryData LEDFHackAutomation)
    <|> Parsec.try (pLogEntryData LEBattleMiss1)
    <|> Parsec.try (pLogEntryData LEBattleMiss2)
    <|> Parsec.try (pLogEntryData LEBattleEvent1)
    <|> Parsec.try (pLogEntryData LEBattleEvent2)
    <|> Parsec.try (pLogEntryData LEBattleStrike)
    <|> Parsec.try (pLogEntryData LESystem1)
    <|> Parsec.try (pLogEntryData LESystem2)
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

