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

import Control.Exception ( throw )
import Control.Lens ( (?~), (.~), (&) )
import Data.Text ( pack, unpack, Text )
import qualified Data.Text as T
import Text.Parsec
import Data.Maybe

import LogParser.LogEntry
import LogParser.Rules.Helpers


-- | Parsing rules for each LogEntryTag constructor
pLogEntryData :: LogEntryTag -> Parsec Text LogParseConfig LogEntryData
pLogEntryData LEDefault = do
    w <- pMany1 anyChar
    --parserTrace "label1"
    return $ newLogEntryData & strs .~ [w] 
pLogEntryData t@LECraftCancel = do
    acA <- pActor ["cancels"]
    string "cancels "
    jobS <- pTillChars ":"
    string " Needs "
    matS <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobSuspensionBuilding = do
    acA <- pActor ["cancels"]
    string "cancels "
    jobS <- pString "Construct Building"
    string ": "
    wA <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ jobS
        & strs .~ [wA] 
pLogEntryData t@LEJobSuspensionLinkage = do
    string "The dwarves suspended a "
    jobS <- pString "linkage from"
    space
    matS <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobSuspensionConstruction = do
    string "The dwarves suspended the "
    jobS <- pString "construction"
    string " of "
    matS <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & job ?~ jobS
        & mat ?~ matS
pLogEntryData t@LEJobCancel = do
    acA <- pActor ["cancels"]
    string "cancels "
    jobS <- pMany1 (noneOf [':'])
    string ": "
    wA <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ jobS
        & strs .~ [wA] 
pLogEntryData t@LEProductionCompleted = do
    jobS <- pTillChars "("
    matS <- pMany1 digit
    char ')'
    string " has been completed."
    return $ newLogEntryData & tag .~ t
        & job ?~ T.init jobS
        & mat ?~ matS
pLogEntryData t@LEMasterpieceImproved = do
    acA <- pActor ["has"]
    string "has improved a "
    matS <- pSomething ["masterfully!"]
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & mat ?~ matS
pLogEntryData t@LEDeathFound = do
    acA <- pActor ["has"]
    string "has been found dead"
    wA <- try 
        (lookAhead (char '.') >> return "")
        <|> (char ',' >> space >> pMany1 (noneOf ['.']))
    char '.'
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [wA]
pLogEntryData t@LECrimeTheft = do
    matS <- pFullName
    wA <- pString "is missing from its proper place!"
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
        & strs .~ [wA] 
pLogEntryData t@LEDFHackAutomation = do
    string "Marked "
    numS <- pMany1 digit
    string " items "
    wA <- pack <$> try (string "to" <|> string "for")
    space
    jobS <- pAny
    return $ newLogEntryData & tag .~ t
        & mat ?~ numS
        & job ?~ jobS
        & strs .~ [wA] 
pLogEntryData t@LEBattleBlock = do
    string "The "
    someoneA <- pSomeone ["attacks", "strikes"]
    wA <- pString "attacks" <|> pString "strikes at"
    string " the "
    someoneB <- pSomeone ["but"]
    wB <- pAny
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature someoneA
        & ac2 ?~ Creature someoneB
        & strs .~ [wA, tc<>ts<>wB] 
pLogEntryData t@LEBattleMiss = do
    string "The "
    someoneA <- pSomeone ["misses"]
    wA <- pString "misses"
    string " the "
    someoneB <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature someoneA
        & ac2 ?~ Creature someoneB
        & strs .~ [wA] 
pLogEntryData t@LEBattleEvent = do
    try ( do
        w1' <- pString "The"
        spaces
        (w2s', a1', a2') <- try (do 
                someoneA <- pSomeone ["charges", "collides"]
                w1 <- try (pString "charges at") <|> pString "collides with"
                string " the "
                someoneB <- pTillChars "!"
                return ([w1, texcl], Just someoneA, Just someoneB)
                )
            <|> try ( do
                someoneA <- pSomeone ["has", "is", "stands", "passes", "falls", "regains"]
                w1 <- try (pString "has been stunned")            
                    <|> try (pString "is knocked over")
                    <|> try (pString "has been knocked unconscious") 
                    <|> try (pString "stands up")  
                    <|> try (pString "passes out")                   
                    <|> try (pString "falls over")
                    <|> try (pString "regains consciousness")        
                    <|> pString "is no longer stunned"
                w2 <- pack . (:[]) <$> (char '!' <|> char '.')
                return ([w1<>w2, T.empty], Just someoneA, Nothing)
                )
            <|> try ( do
                someoneA <- pSomeone ["bounces"]
                w1 <- pString "bounces backward!"
                return ([w1, T.empty], Just someoneA, Nothing)
                )
            <|> try ( do
                someoneA <- pSomeone ["collapses"]
                w1 <- pString "collapses and falls to the ground from over-exertion."
                return ([w1, T.empty], Just someoneA, Nothing)
                )
            <|> ( do
                someoneA <- pSomeone ["looks"]
                w1 <- pString "looks surprised by the ferocity of"
                w2 <- option "" (pString "The ")
                someoneB <- pSomeone ["onslaught"]
                w3 <- pString "onslaught!"
                return ([w1<>ts<>w2, w3], Just someoneA, Just someoneB)
                )
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ maybe Nobody Creature a1'
            & ac2 ?~ maybe Nobody Creature a2'
            & strs .~ (w1' : w2s')
        )
    <|> ( do
        w1 <- pString "They tangle together and "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2]
        )
pLogEntryData t@LEBattleStrike = do
    string "The "
    someoneA <- pSomeone 
        ["leaps","punches","punches","catches","snatches","stabs"
        ,"grabs","hacks","pushes","misses","slashes","shakes"
        ,"blocks","gores","strangles","strikes","scratches","kicks"
        ,"attacks","lashes","slaps","bashes","bites","strikes"
        ,"punches","releases","throws","takes","locks","bends"
        ,"places","gouges"]
    wA <- try (pString "leaps at")<|> try (pString "punches")  <|> try (pString "punches")
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
    wB <- pAny
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature someoneA
        & ac2 ?~ Creature someoneB
        & strs .~ [wA, wB]
pLogEntryData t@LEBattleHit = do
    w1'' <- pString "The "
    w2s'' <- try ( do
            w1' <- pTillChars ","
            space
            w2s' <- try ( do
                    w1 <- try (pString "bruising") <|> try (pString "chipping") 
                        <|> try (pString "shattering") 
                        <|> try (pString "fracturing") 
                        <|> pString "denting"
                    space
                    w2 <- try (pString "it") <|> pString "the"
                    w3 <- pAny
                    return [w1, ts, w2, w3]
                    )
                <|> try ( do
                    w1 <- pString "jamming the "
                    w2 <- pSomething ["through"]
                    w3 <- pString "through the"
                    w4 <- pAny
                    return [w1, w2, w3, w4]
                    )
                <|> ( do
                    w1 <- pString "tearing "
                    w2 <- try (pString "apart the") <|> try (pString "it apart")
                        <|> pString "the"
                    w3 <- pAny
                    return [w1, w2, w3]
                    )
            return (w1':tc:ts:w2s')
            )
        <|> try ( do
            w1' <- pSomething ["and", "takes", "pulls", "strikes"]
            w2' <- try (pString "and the severed part") 
                <|> try (pString "takes the full force")
                <|> try (pString "pulls the") 
                <|> pString "strikes The"
            w3' <- pAny
            return [w1', ts, w2', w3']
            )
        <|> ( do
            w1' <- pString "flying "
            w2' <- pSomething ["strikes"]
            w3' <- pAny
            return [w1', w2', w3']
            )
    return $ newLogEntryData & tag .~ t
        & strs .~ [T.concat (w1'':w2s'')]
pLogEntryData t@LEBattleEvade = do
    try ( do
        w1' <- option "" (pString "The")
        spaces
        (w1s', a1', a2') <- try ( do
                a <- pActor ["jumps","scrambles","rolls","falls","bats"]
                w1 <- try (pString "jumps away")
                    <|> try (pString "jumps out")
                    <|> try (pString "jump away")
                    <|> try (pString "scrambles out of")
                    <|> try (pString "scrambles away")
                    <|> try (pString "rolls out of")
                    <|> try (pString "rolls away")
                    <|> try (pString "bats The")
                    <|> pString "falls over"
                w2 <- pAny
                return ([w1<>w2, T.empty], a, Nothing)
                )
            <|> try ( do 
                a <- pActor ["blocks","miss"]
                w1 <- try (pString "blocks")
                    <|> pString "miss"
                w2 <- pAny
                return ([w1<>w2, T.empty], a, Nothing)
                )
            <|> try ( do 
                a1 <- pActor ["strikes"]
                w1 <- pString "strikes at"
                a2 <- pActor ["but"]
                w2 <- pString "but the shot is blocked"
                w3 <- pAny
                return ([w1, w2<>w3], a1, Just a2)
                )
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ a1'
            & ac2 .~ (Just =<< a2')
            & strs .~ w1':w1s'
        )
pLogEntryData t@LEBattleStatus = do
    try ( do
        (dA'', w1s'') <- try ( do
                option "" (pString "The ")
                dA <- pActor [ "skids", "has", "is", "gives", "passes", "looks"
                            , "vomits", "retches", "regains" ]
                w2 <- try (pString "skids along the ground!")
                    <|> try (pString "has been stunned")
                    <|> try (pString "is no longer stunned")
                    <|> try (pString "gives in to pain")
                    <|> try (pString "passes out")
                    <|> try (pString "looks sick")
                    <|> try (pString "looks even more sick")
                    <|> try (pString "vomits")
                    <|> try (pString "retches")
                    <|> try (pString "is having trouble breathing")
                    <|> try (pString "is having more trouble breathing")
                    <|> try (pString "has become enraged")
                    <|> try (pString "is no longer enraged")
                    <|> try (pString "has been knocked unconscious")
                    <|> try (pString "passes out from exhaustion")
                    <|> try (pString "regains consciousness")
                    <|> try (pString "is feeling sluggish")
                    <|> try (pString "looks numb")
                    <|> try (pString "is partially paralyzed")
                    <|> try (pString "is completely paralyzed")
                    <|> try (pString "has fully overcome the paralysis")
                    <|> try (pString "has been bitten by")
                    <|> pString "is injected into the"
                w3 <- pAny
                return (dA, [w2, w3])
                )
            <|> try ( do
                option "" (pString "The ")
                dA <- pActor ["cancels"]
                w2 <- pString "cancels"
                w3 <- pTillChars ":"
                w4 <- try (pString " Paralyzed.") 
                    <|> pString " Resting injury."
                return (dA, [w2, w3, w4])
                )
            <|> try ( do
                w1' <- pack . (:[]) <$> oneOf "tT"
                w2' <- pString "he "
                (dA', w3s') <- try ( do
                        dA <- pActor ["vomits", "retches", "looks", "trouble"]
                        w2 <- try (pString "vomits")
                            <|> try (pString "retches")
                            <|> try (pString "looks sick")
                            <|> try (pString "looks even more sick")
                            <|> try (pString "trouble breathing")
                        w3 <- pAny
                        return (dA, [w2, w3])
                        )
                    <|> try ( do
                        dA <- pActor ["skips", "rolls", "unrolls"]
                        w2 <- try (pString "skips across the water!")
                            <|> try (pString "rolls into a ball.")
                            <|> pString "unrolls."
                        return (dA, [w2])
                        )
                return (dA', w1':w2':w3s')
                )
        return $ newLogEntryData & tag .~ t
            & strs .~ [T.concat w1s'']
            & ac1 ?~ dA''
        )
    <|> ( do
        w1 <- pString "The vomit "
        w2 <- try (pString "disappears into the water.")
            <|> try (pString "burns away in the lava.")
            <|> pString "splatters into the shallow puddle of water."
        return $ newLogEntryData & tag .~ t
            & strs .~ [T.concat [w1, w2]]
        )
pLogEntryData t@LEGore = do
    try ( do
            w1 <- try (pString "A ") <|> pString "An "
            w2 <- pSomething ["has"]
            w3 <- pString "has been "
            w4 <- try (pString "severed") <|> try (pString "torn")
                <|> try (pString "opened") <|> try (pString "strained")
                <|> pString "bruised"
            w5  <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2<>ts<>w3<>w4<>w5]
            )
    <|> try ( do
            w1 <- pString "A ligament in "
            w2 <- pSomething ["has"]
            w3 <- pString "has been"
            w5 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2<>ts<>w3]
            )
    <|> try ( do
            w1 <- pString "The "
            w2 <- pSomething ["has"]
            w3 <- pString "has lodged firmly in the wound!"
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2<>ts<>w3]
            )
    <|> try ( do
            w1 <- pString "The "
            w2 <- pSomething ["is"]
            w3 <- pString "is smashed into the "
            w4 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2<>ts<>w3<>w4]
            )
    <|> try ( do
            w1 <- pSomething ["pulls"]
            w2 <- try (pString "pulls on the embedded") 
                    <|> pString "pulls out and releases the"
            w3 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2<>w3]
            )
    <|> try ( do
            w1 <- pSomething ["in"]
            w2 <- pString "in the "
            w3 <- pSomething ["with"]
            w4 <- pString "with "
            w5 <- pSomething ["and"]
            w6 <- pString "and the injured part is cloven asunder!"
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2<>w3<>ts<>w4<>w5<>ts<>w6]
            )
    <|> try ( do
            w1 <- pSomething ["blood"]
            w2 <- pString "blood is sucked out of the wound!"
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2]
            )
    <|> try ( do
            w1 <- pString "The guts pops out of the wound!"
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1]
            )
    <|> try ( do
            w1 <- pString "Many nerves have been severed "
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2]
            )
    <|> try ( do
            w1 <- pString "The "
            w2 <- pSomething ["gouges"]
            w3 <- pString "gouges The "
            w4 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2<>ts<>w3<>w4]
            )
    <|> ( do
            w1 <- pSomething ["twists"]
            w2 <- pString "twists the embedded "
            w3 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2<>w3]
            )
pLogEntryData t@LEAnimalGrown = do
    string "An animal has grown to become a "
    matS <- pAny
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
pLogEntryData t@LEAnimalBirth =
    try ( do
        s1 <- pSomeone ["has"]
        w1 <- pString "has given birth to "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & mat ?~ s1
            & strs .~ [w1<>w2]
        )
    <|> ( do
        s1 <- pSomeone ["have"]
        w1 <- pString "have hatched."
        return $ newLogEntryData & tag .~ t
            & mat ?~ s1
            & strs .~ [w1]
        )
pLogEntryData t@LEWeather = do
    wA <- try (pString "It has started raining.") 
        <|> try ( do
                wA' <- pString "It is raining "
                wA'' <- pAny
                return $ wA'<>wA''
                )
        <|> try (do 
                wA' <- pString "A cloud of "
                wA'' <- pSomething ["has"]
                wA''' <- pString "has drifted nearby!"
                return $ wA'<>wA''<>ts<>wA'''
                )
        <|> try (pString "A snow storm has come.")
        <|> pString "The weather has cleared."
    return $ newLogEntryData & tag .~ t
        & strs .~ [wA]
pLogEntryData t@LESeason = do
    wA <- try ( do
            wA' <- pMany1 (noneOf [' '])
            wA'' <- pString " has arrived"
            wA''' <- pAny
            return $ wA'<>wA''<>wA'''
            )
        <|> try (pString "It is now summer.")
        <|> try (pString "Autumn has come.")
        <|> try (pString "Winter is upon you.")
        <|> try (pString "The wet season has arrived!")
        <|> try (pString "The dry season has come")
        <|> pString "Nothing has arrived on the calendar."
    return $ newLogEntryData & tag .~ t
        & strs .~ [wA]
pLogEntryData t@LESystem =
    try ( do
            wA' <- try (pString "Loaded ") <|> pString "**"
            wA'' <- pAny
            let wA = wA'<>wA''
            return $ newLogEntryData & tag .~ t
                & strs .~ [wA]
        )
    <|> ( do
        wA' <- pChar 'x'
        wA'' <- pMany1 digit
        return $ newLogEntryData & tag .~ t
            & strs .~ [wA'<>wA'']
        )

-- *****************************************************************************

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
    <|> try (pLogEntryData LEBattleEvent)
    <|> try (pLogEntryData LEBattleStrike)
    <|> try (pLogEntryData LEBattleHit)
    <|> try (pLogEntryData LEBattleEvade)
    <|> try (pLogEntryData LEBattleStatus)
    <|> try (pLogEntryData LEGore)
    <|> try (pLogEntryData LEAnimalGrown)
    <|> try (pLogEntryData LEAnimalBirth)
    <|> try (pLogEntryData LEWeather)
    <|> try (pLogEntryData LESeason)
    <|> try (pLogEntryData LESystem)
    <|> pLogEntryData LEDefault

-- | Parse one log entry
parseLogEntry :: LogParseConfig -> Text -> LogEntryData
parseLogEntry lpCfg txt = 
    case runParser baseRule lpCfg "" txt of
        (Right v)   -> v
        (Left s)    -> throw $ ExLogParse s

-- | Parse one log entry with specific rule. Used in tests
parseLogEntrySingle :: LogParseConfig 
    -> Parsec Text LogParseConfig LogEntryData -> Text -> LogEntryData
parseLogEntrySingle lpCfg rule txt = 
    case runParser rule lpCfg "" txt of
        (Right v)   -> v
        (Left s)    -> throw $ ExLogParse s

