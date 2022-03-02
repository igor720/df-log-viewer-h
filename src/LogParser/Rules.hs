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
import qualified Data.Text as T
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

pLineEnd :: Parsec.Parsec Text LogParseConfig ()
pLineEnd = do
    Parsec.optional Parsec.endOfLine

pTillChars :: String -> Parsec.Parsec Text LogParseConfig Text
pTillChars chars = do
    s <- pMany1 (Parsec.noneOf chars)
    Parsec.oneOf chars
    return s

pGameEntityPart :: Parsec.Parsec Text LogParseConfig String
pGameEntityPart = do
    a <- Parsec.upper
    ss <- Parsec.many1 (Parsec.noneOf [' ']) -- Parsec.letter
    Parsec.spaces
    return $ a : ss

pGameEntity :: Parsec.Parsec Text LogParseConfig Text
pGameEntity  = do
    gameEntity <- Parsec.manyTill pGameEntityPart 
        (Parsec.try (Parsec.lookAhead (Parsec.lower <|> Parsec.oneOf ",:.")))
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
    nameS <- pMany1 (Parsec.noneOf [','])
    Parsec.string ", "
    Dorf nameS nickname <$> pGameEntity

pDorfAny :: [String] -> Parsec.Parsec Text LogParseConfig Dorf
pDorfAny endWith = do
    Parsec.try pDorfFull
    <|> ( do
        nameS <- pSomeone endWith
        Parsec.char ' '
        return $ Dorf nameS Nothing ""
        )

pSomeone :: [String] -> Parsec.Parsec Text LogParseConfig Text
pSomeone endWith = do
    str <- Parsec.lookAhead (Parsec.many (Parsec.noneOf "\r,:."))
    let ws = L.takeWhile (`notElem` endWith) $ L.words str
        someone = L.unwords ws
    Parsec.string someone
    return $ pack someone

pSomething :: [String] -> Parsec.Parsec Text LogParseConfig Text
pSomething = pSomeone

-- ****************************************************************************

-- | Parsing rules for each LogEntryTag constructor
pLogEntryData :: LogEntryTag -> Parsec.Parsec Text LogParseConfig LogEntryData
pLogEntryData LEDefault = do
    warn <- pMany1 (Parsec.noneOf ['\r'])
    --Parsec.parserTrace "label1"
    pLineEnd
    return $ newLogEntryData & warns .~ [warn] 
pLogEntryData t@LECraftCancel = do
    dorfA <- pDorfAny ["cancels"]
    Parsec.string "cancels "
    jobS <- pTillChars ":"
    Parsec.string " Needs "
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobSuspensionBuilding = do
    dorfA <- pDorfAny ["cancels"]
    Parsec.string "cancels "
    jobS <- pString "Construct Building"
    Parsec.string ": "
    warnA <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & warns .~ [warnA] 
pLogEntryData t@LEJobSuspensionLinkage = do
    Parsec.string "The dwarves suspended a "
    jobS <- pString "linkage from"
    Parsec.space
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobSuspensionConstruction = do
    Parsec.string "The dwarves suspended the "
    jobS <- pString "construction"
    Parsec.string " of "
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobCancel = do
    dorfA <- pDorfAny ["cancels"]
    Parsec.string "cancels "
    jobS <- pMany1 (Parsec.noneOf [':'])
    Parsec.string ": "
    warnA <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & warns .~ [warnA] 
pLogEntryData t@LEProductionCompleted = do
    jobS <- pTillChars "("
    matS <- pMany1 Parsec.digit
    Parsec.char ')'
    Parsec.string " has been completed."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & job ?~ T.init jobS
        & mat ?~ matS
pLogEntryData t@LEMasterpieceImproved = do
    dorfA <- pDorfAny ["has"]
    Parsec.string "has improved a "
    matS <- pSomething ["masterfully!"]
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & mat ?~ matS
pLogEntryData t@LEDeathFound = do
    dorfA <- pDorfAny ["has"]
    Parsec.string "has been found dead"
    warnA <- Parsec.try 
        (Parsec.lookAhead (Parsec.char '.') >> return "")
        <|> (Parsec.char ',' >> Parsec.space >> pMany1 (Parsec.noneOf ['.']))
    Parsec.char '.'
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & warns .~ [warnA]
pLogEntryData t@LECrimeTheft = do
    matS <- pGameEntity
    warnA <- pString "is missing from its proper place!"
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
        & warns .~ [warnA] 
pLogEntryData t@LEDFHackAutomation = do
    Parsec.string "Marked "
    numS <- pMany1 Parsec.digit
    Parsec.string " items "
    warnA <- pack <$> Parsec.try (Parsec.string "to" <|> Parsec.string "for")
    Parsec.space
    jobS <- pMany1 (Parsec.noneOf ['\r'])
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & mat ?~ numS
        & job ?~ jobS
        & warns .~ [warnA] 
pLogEntryData t@LEBattleBlock = do
    Parsec.string "The "
    someoneA <- pSomeone ["attacks", "strikes"]
    Parsec.space
    warnA <- pString "attacks" <|> pString "strikes at"
    Parsec.string " the "
    someoneB <- pSomeone ["but"]
    Parsec.space
    warnB <- pTillChars "!"
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA, warnB] 
pLogEntryData t@LEBattleMiss = do
    Parsec.string "The "
    someoneA <- pSomeone ["misses"]
    Parsec.space
    warnA <- pString "misses"
    Parsec.string " the "
    someoneB <- pTillChars "!"
    pLineEnd
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
    someoneB <- pTillChars "!"
    pLineEnd
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
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & warns .~ [warnA]
pLogEntryData t@LEBattleStrike = do
    Parsec.string "The "
    someoneA <- pSomeone 
        ["leaps","punches","punches","catches","snatches","stabs"
        ,"grabs","hacks","pushes","misses","slashes","shakes"
        ,"blocks","gores","strangles","strikes","scratches","kicks"
        ,"attacks","lashes","slaps","bashes","bites","strikes"
        ,"punches","releases","throws","takes","locks","bends"
        ,"places","gouges"]
    Parsec.space
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
    Parsec.string " the "
    someoneB <- pSomeone ["in"]
    warnB <- pMany1 (Parsec.noneOf ['\r'])
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA, warnB]
pLogEntryData t@LEAnimalGrown = do
    Parsec.string "An animal has grown to become a "
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
pLogEntryData t@LEWeather = do
    warnA <- Parsec.try (pString "It has started raining.") 
        <|> Parsec.try ( do
                warnA' <- Parsec.string "It is raining "
                warnA'' <- Parsec.many1 (Parsec.noneOf ".!")
                warnA''' <- Parsec.oneOf ".!"
                return $ pack $ warnA'<>warnA''<>[warnA''']
                )
        <|> Parsec.try (do 
                warnA' <- Parsec.string "A cloud of "
                warnA'' <- unpack <$> pSomething ["has"]
                warnA''' <- Parsec.string " has drifted nearby!"
                return $ pack $ warnA'<>warnA''<>warnA'''
                )
        <|> Parsec.try (pString "A snow storm has come.")
        <|> pString "The weather has cleared."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]
pLogEntryData t@LESeason = do
    warnA <- Parsec.try ( do
            warnA' <- Parsec.many1 (Parsec.noneOf [' '])
            warnA'' <- Parsec.string " has arrived"
            warnA''' <- Parsec.many (Parsec.noneOf ['\r'])
            return $ pack $ warnA'<>warnA''<>warnA'''
            )
        <|> Parsec.try (pString "It is now summer.")
        <|> Parsec.try (pString "Autumn has come.")
        <|> Parsec.try (pString "Winter is upon you.")
        <|> Parsec.try (pString "The wet season has arrived!")
        <|> Parsec.try (pString "The dry season has come")
        <|> pString "Nothing has arrived on the calendar."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]
pLogEntryData t@LESystem = do
    warnA' <- Parsec.try (pString "Loaded ") <|> pString "**"
    warnA'' <- pMany1 (Parsec.noneOf ['\r'])
    let warnA = warnA'<>warnA''
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]


-- | Base parsing rule; place move specific and more friquent rules to top
baseRule :: Parsec.Parsec Text LogParseConfig LogEntryData
baseRule = 
        Parsec.try (pLogEntryData LECraftCancel)
    <|> Parsec.try (pLogEntryData LEJobSuspensionBuilding)
    <|> Parsec.try (pLogEntryData LEJobSuspensionLinkage)
    <|> Parsec.try (pLogEntryData LEJobSuspensionConstruction)
    <|> Parsec.try (pLogEntryData LEJobCancel)
    <|> Parsec.try (pLogEntryData LEProductionCompleted)
    <|> Parsec.try (pLogEntryData LEMasterpieceImproved)
    <|> Parsec.try (pLogEntryData LEDeathFound)
    <|> Parsec.try (pLogEntryData LECrimeTheft)
    <|> Parsec.try (pLogEntryData LEDFHackAutomation)
    <|> Parsec.try (pLogEntryData LEBattleBlock)
    <|> Parsec.try (pLogEntryData LEBattleMiss)
    <|> Parsec.try (pLogEntryData LEBattleEvent1)
    <|> Parsec.try (pLogEntryData LEBattleEvent2)
    <|> Parsec.try (pLogEntryData LEBattleStrike)
    <|> Parsec.try (pLogEntryData LEAnimalGrown)
    <|> Parsec.try (pLogEntryData LEWeather)
    <|> Parsec.try (pLogEntryData LESeason)
    <|> Parsec.try (pLogEntryData LESystem)
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

