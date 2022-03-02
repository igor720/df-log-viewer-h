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

import Control.Lens ( (?~), (.~), (&), Identity )
import Data.Text ( pack, unpack, Text )
import qualified Data.Text as T
import qualified Data.List as L
import Text.Parsec

import LogException
import LogParser.LogEntry


data LogParseConfig = LogParseConfig    -- empty for now
        deriving Show

ts :: Text
ts = pack " "

texcl :: Text
texcl = pack "!"

tp :: Text
tp = pack "."

pMany :: (Stream Text Identity t) => ParsecT Text u Identity Char 
    -> ParsecT Text u Identity Text
pMany p = pack <$> many p

pMany1 :: (Stream Text Identity t) => ParsecT Text u Identity Char 
    -> ParsecT Text u Identity Text
pMany1 p = pack <$> many1 p

pString :: (Stream Text Identity t) => String 
    -> ParsecT Text u Identity Text
pString s = pack <$> string s

pWord :: (Stream Text Identity t) => ParsecT Text u Identity Text
pWord = pack <$> many1 (noneOf [' '])

pLineEnd :: Parsec Text LogParseConfig ()
pLineEnd = do
    optional endOfLine

pTillChars :: String -> Parsec Text LogParseConfig Text
pTillChars chars = do
    s <- pMany (noneOf chars)
    oneOf chars
    return s

pGameEntityPart :: Parsec Text LogParseConfig String
pGameEntityPart = do
    a <- upper
    ss <- many1 (noneOf [' ']) -- letter
    spaces
    return $ a : ss

pGameEntity :: Parsec Text LogParseConfig Text
pGameEntity  = do
    gameEntity <- manyTill pGameEntityPart 
        (try (lookAhead (lower <|> oneOf ",:.")))
    return $ pack (L.unwords gameEntity)

pDorfFull :: Parsec Text LogParseConfig Dorf
pDorfFull = do
    nicknameStartMb <- optionMaybe (char '`')
    nickname <- mapM (\_ -> do
            str <- many1 (noneOf ['\''])
            _ <- char '\''
            spaces 
            return $ pack str
        ) nicknameStartMb
    nameS <- pMany1 (noneOf [','])
    string ", "
    Dorf nameS nickname <$> pGameEntity

pDorfAny :: [String] -> Parsec Text LogParseConfig Dorf
pDorfAny endWith = do
    try pDorfFull
    <|> ( do
        nameS <- pSomeone endWith
        return $ Dorf nameS Nothing ""
        )

pSomeone :: [String] -> Parsec Text LogParseConfig Text
pSomeone endWith = do
    s <- manyTill (noneOf "\r")
            (try (lookAhead (choice 
                (map (try . string) endWith)
            )))
    return $ pack (if null s then s else init s)

pSomething :: [String] -> Parsec Text LogParseConfig Text
pSomething = pSomeone

-- ****************************************************************************

-- | Parsing rules for each LogEntryTag constructor
pLogEntryData :: LogEntryTag -> Parsec Text LogParseConfig LogEntryData
pLogEntryData LEDefault = do
    warn <- pMany1 (noneOf ['\r'])
    --parserTrace "label1"
    pLineEnd
    return $ newLogEntryData & warns .~ [warn] 
pLogEntryData t@LECraftCancel = do
    dorfA <- pDorfAny ["cancels"]
    string "cancels "
    jobS <- pTillChars ":"
    string " Needs "
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobSuspensionBuilding = do
    dorfA <- pDorfAny ["cancels"]
    string "cancels "
    jobS <- pString "Construct Building"
    string ": "
    warnA <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & warns .~ [warnA] 
pLogEntryData t@LEJobSuspensionLinkage = do
    string "The dwarves suspended a "
    jobS <- pString "linkage from"
    space
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobSuspensionConstruction = do
    string "The dwarves suspended the "
    jobS <- pString "construction"
    string " of "
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobCancel = do
    dorfA <- pDorfAny ["cancels"]
    string "cancels "
    jobS <- pMany1 (noneOf [':'])
    string ": "
    warnA <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & job ?~ jobS
        & warns .~ [warnA] 
pLogEntryData t@LEProductionCompleted = do
    jobS <- pTillChars "("
    matS <- pMany1 digit
    char ')'
    string " has been completed."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & job ?~ T.init jobS
        & mat ?~ matS
pLogEntryData t@LEMasterpieceImproved = do
    dorfA <- pDorfAny ["has"]
    string "has improved a "
    matS <- pSomething ["masterfully!"]
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ dorfA
        & mat ?~ matS
pLogEntryData t@LEDeathFound = do
    dorfA <- pDorfAny ["has"]
    string "has been found dead"
    warnA <- try 
        (lookAhead (char '.') >> return "")
        <|> (char ',' >> space >> pMany1 (noneOf ['.']))
    char '.'
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
    string "Marked "
    numS <- pMany1 digit
    string " items "
    warnA <- pack <$> try (string "to" <|> string "for")
    space
    jobS <- pMany1 (noneOf ['\r'])
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & mat ?~ numS
        & job ?~ jobS
        & warns .~ [warnA] 
pLogEntryData t@LEBattleBlock = do
    string "The "
    someoneA <- pSomeone ["attacks", "strikes"]
    warnA <- pString "attacks" <|> pString "strikes at"
    string " the "
    someoneB <- pSomeone ["but"]
    warnB <- pTillChars "!"
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA, warnB] 
pLogEntryData t@LEBattleMiss = do
    string "The "
    someoneA <- pSomeone ["misses"]
    warnA <- pString "misses"
    string " the "
    someoneB <- pTillChars "!"
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA] 
pLogEntryData t@LEBattleEvent1 = do
    string "The "
    someoneA <- pSomeone ["charges", "collides"]
    warnA' <- char 'c'
    warnA''<- string "harges at" <|> string "ollides with"
    let warnA = pack $ warnA':warnA''
    string " the "
    someoneB <- pTillChars "!"
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA]
pLogEntryData t@LEBattleEvent2 = do
    string "The "
    someoneA <- pSomeone ["has", "is", "stands", "passes", "falls", "regains"]
    warnA <- try (pString "has been stunned")            <|> try (pString "is knocked over")
        <|> try (pString "has been knocked unconscious") <|> try (pString "stands up")  
        <|> try (pString "passes out")                   <|> try (pString "falls over")
        <|> try (pString "regains consciousness")        <|> pString "is no longer stunned"
    char '!' <|> char '.'
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & warns .~ [warnA]
pLogEntryData t@LEBattleStrike = do
    string "The "
    someoneA <- pSomeone 
        ["leaps","punches","punches","catches","snatches","stabs"
        ,"grabs","hacks","pushes","misses","slashes","shakes"
        ,"blocks","gores","strangles","strikes","scratches","kicks"
        ,"attacks","lashes","slaps","bashes","bites","strikes"
        ,"punches","releases","throws","takes","locks","bends"
        ,"places","gouges"]
    warnA <- try (pString "leaps at")<|> try (pString "punches")  <|> try (pString "punches")
        <|> try (pString "catches")  <|> try (pString "snatches at") <|> try (pString "stabs")
        <|> try (pString "grabs")    <|> try (pString "hacks")    <|> try (pString "pushes")
        <|> try (pString "misses")   <|> try (pString "slashes")  <|> try (pString "shakes")
        <|> try (pString "blocks")   <|> try (pString "gores")    <|> try (pString "strangles")
        <|> try (pString "strikes")  <|> try (pString "scratches")<|> try (pString "kicks")
        <|> try (pString "attacks")  <|> try (pString "lashes")   <|> try (pString "slaps")
        <|> try (pString "bashes")   <|> try (pString "bites")    <|> try (pString "strikes at")
        <|> try (pString "punches")  <|> try (pString "releases") <|> try (pString "throws")
        <|> try (pString "takes")    <|> try (pString "locks")    <|> try (pString "bends")
        <|> try (pString "places a chokehold on")                 <|> pString "gouges"
    string " the "
    someoneB <- pSomeone ["in"]
    warnB <- pMany1 (noneOf ['\r'])
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & dorf1 ?~ Dorf someoneA Nothing ""
        & dorf2 ?~ Dorf someoneB Nothing ""
        & warns .~ [warnA, warnB]
pLogEntryData t@LEGore = do
    try ( do
            w1 <- try (pString "A ") <|> pString "An "
            w2 <- pSomething ["has"]
            w3 <- pString "has been "
            w4 <- try (pString "severed") <|> try (pString "torn")
                <|> try (pString "opened") <|> try (pString "strained")
                <|> pString "bruised"
            w5  <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>w2<>ts<>w3<>w4<>w5<>texcl]
            )
        <|> ( do
            w1 <- pString "A ligament in "
            w2 <- pSomething ["has"]
            w3 <- pString "has been"
            w5  <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>w2<>ts<>w3<>texcl]
            )
        <|> ( do
            w1 <- pString "The "
            w2 <- pSomething ["has"]
            w3 <- pString "has lodged firmly in the wound!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>w2<>ts<>w3]
            )
        <|> ( do
            w1 <- pString "The "
            w2 <- pSomething ["is"]
            w3 <- pString "is smashed into the "
            w4 <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>w2<>ts<>w3<>w4<>texcl]
            )
        <|> ( do
            w1 <- pSomething ["pulls"]
            w2 <- try (pString "pulls on the embedded") 
                    <|> pString "pulls out and releases the"
            w3 <- pTillChars "."
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>ts<>w2<>w3<>tp]
            )
        <|> ( do
            w1 <- pSomething ["in"]
            w2 <- pString "in the "
            w3 <- pSomething ["with"]
            w4 <- pString "with "
            w5 <- pSomething ["and"]
            w6 <- pString "and the injured part is cloven asunder!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>ts<>w2<>w3<>ts<>w4<>w5<>ts<>w6]
            )
        <|> ( do
            w1 <- pSomething ["blood"]
            w2 <- pString "blood is sucked out of the wound!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>ts<>w2]
            )
        <|> ( do
            w1 <- pString "The guts pops out of the wound!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1]
            )
        <|> ( do
            w1 <- pString "Many nerves have been severed "
            w2 <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>w2<>texcl]
            )
        <|> ( do
            w1 <- pString "The "
            w2 <- pSomething ["gouges"]
            w3 <- pString "gouges The "
            w4 <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>w2<>ts<>w3<>w4<>texcl]
            )
        <|> ( do
            w1 <- pSomething ["twists"]
            w2 <- pString "twists the embedded "
            w3 <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & warns .~ [w1<>ts<>w2<>w3<>texcl]
            )
pLogEntryData t@LEAnimalGrown = do
    string "An animal has grown to become a "
    matS <- pTillChars "."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
pLogEntryData t@LEWeather = do
    warnA <- try (pString "It has started raining.") 
        <|> try ( do
                warnA' <- string "It is raining "
                warnA'' <- many1 (noneOf ".!")
                warnA''' <- oneOf ".!"
                return $ pack $ warnA'<>warnA''<>[warnA''']
                )
        <|> try (do 
                warnA' <- string "A cloud of "
                warnA'' <- unpack <$> pSomething ["has"]
                warnA''' <- string "has drifted nearby!"
                return $ pack $ warnA'<>warnA''<>[' ']<>warnA'''
                )
        <|> try (pString "A snow storm has come.")
        <|> pString "The weather has cleared."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]
pLogEntryData t@LESeason = do
    warnA <- try ( do
            warnA' <- many1 (noneOf [' '])
            warnA'' <- string " has arrived"
            warnA''' <- many (noneOf ['\r'])
            return $ pack $ warnA'<>warnA''<>warnA'''
            )
        <|> try (pString "It is now summer.")
        <|> try (pString "Autumn has come.")
        <|> try (pString "Winter is upon you.")
        <|> try (pString "The wet season has arrived!")
        <|> try (pString "The dry season has come")
        <|> pString "Nothing has arrived on the calendar."
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]
pLogEntryData t@LESystem = do
    warnA' <- try (pString "Loaded ") <|> pString "**"
    warnA'' <- pMany1 (noneOf ['\r'])
    let warnA = warnA'<>warnA''
    pLineEnd
    return $ newLogEntryData & tag .~ t
        & warns .~ [warnA]

-- | Base parsing rule; place move specific and more friquent rules to top
baseRule :: Parsec Text LogParseConfig LogEntryData
baseRule = 
        try (pLogEntryData LECraftCancel)
    <|> try (pLogEntryData LEJobSuspensionBuilding)
    <|> try (pLogEntryData LEJobSuspensionLinkage)
    <|> try (pLogEntryData LEJobSuspensionConstruction)
    <|> try (pLogEntryData LEJobCancel)
    <|> try (pLogEntryData LEProductionCompleted)
    <|> try (pLogEntryData LEMasterpieceImproved)
    <|> try (pLogEntryData LEDeathFound)
    <|> try (pLogEntryData LECrimeTheft)
    <|> try (pLogEntryData LEDFHackAutomation)
    <|> try (pLogEntryData LEBattleBlock)
    <|> try (pLogEntryData LEBattleMiss)
    <|> try (pLogEntryData LEBattleEvent1)
    <|> try (pLogEntryData LEBattleEvent2)
    <|> try (pLogEntryData LEBattleStrike)
    <|> try (pLogEntryData LEGore)
    <|> try (pLogEntryData LEAnimalGrown)
    <|> try (pLogEntryData LEWeather)
    <|> try (pLogEntryData LESeason)
    <|> try (pLogEntryData LESystem)
    <|> pLogEntryData LEDefault

parseLogEntry :: LogParseConfig -> Text -> LogEntryData
parseLogEntry lpCfg txt = 
    case runParser baseRule lpCfg "" txt of
        (Right v)   -> v
        (Left s)    -> error $ "parse fail: "++show s

-- Used in tests
parseLogEntrySingle :: LogParseConfig 
    -> Parsec Text LogParseConfig LogEntryData -> Text -> LogEntryData
parseLogEntrySingle lpCfg rule txt = 
    case runParser rule lpCfg "" txt of
        (Right v)   -> v
        (Left s)    -> error $ "parse fail: "++show s

