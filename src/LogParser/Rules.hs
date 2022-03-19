{-|
Module      : LogParser.Rules
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Parsing rules module

################################################################################
## Regular expressions from Petr Prokop (aka 'zwei') `soundsense' DF mod 
## (http://df.zweistein.cz/soundsense/)
## and PeridexisErrant's Starter Pack `Announcement Window' utility filters
## (http://www.bay12forums.com/smf/index.php?topic=126076)
## were partially used as starting points for parsing rules in this module
################################################################################
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
pLogEntryData t@LEJobSuspension = do
    try ( do
        acA <- pActor ["cancels"]
        string "cancels "
        j <- pString "Construct Building"
        string ": "
        w1 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & job ?~ j
            & strs .~ [T.empty, w1, tcol] 
        )
    <|> try ( do
        acA <- pActor ["cancels"]
        string "cancels "
        j <- pString "Link a Building to Trigger"
        string ": "
        w1 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & job ?~ j
            & strs .~ [T.empty, w1, tcol] 
        )
    <|> try ( do
        string "The dwarves were "
        j <- pString "unable to complete the"
        space
        m <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & job ?~ j
            & mat ?~ m
            & strs .~ [T.empty, T.empty, T.empty] 
        )
    <|> try ( do 
        string "The dwarves suspended a "
        j <- pString "linkage from"
        space
        m <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & job ?~ j
            & mat ?~ m
            & strs .~ [T.empty, T.empty, T.empty] 
        )
    <|> ( do 
        string "The dwarves suspended the "
        j <- pString "construction"
        string " of "
        m <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & job ?~ j
            & mat ?~ m
            & strs .~ [T.empty, T.empty, T.empty] 
        )
pLogEntryData t@LECraftCancel = do
    acA <- pActor ["cancels"]
    string "cancels "
    j <- pTillChars ":"
    space
    string "Needs "
    m <- pTillChars "." 
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ j
        & mat ?~ m
pLogEntryData t@LEJobCancel = do
    acA <- pActor ["cancels"]
    string "cancels "
    j <- pMany1 (noneOf [':'])
    string ": "
    wA <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ j
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
pLogEntryData t@LEMasterpieceCreated = do
    acA <- pActor ["has"]
    w1 <- pString "has "
    w2 <- try (pString "created")
            <|> try (pString "cooked")
            <|> try (pString "engraved")
            <|> pString "constructed"
    w3 <- pString " a masterpiece"
    w4 <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1<>w2<>w3<>w4<>texcl] 
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
pLogEntryData t@LEMiningStruck = do
    w1 <- pString "You have struck"
    space
    m <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & mat ?~ m
        & strs .~ [w1] 
pLogEntryData t@LEBattleMiss = do
    try ( do
        w1' <- option "" (pString "The")
        spaces
        (w2s', a1', a2') <- try ( do
                a1 <- pSomeone ["attacks", "strikes"]
                w1 <- try (pString "attacks the") <|> pString "strikes at the"
                space
                a2 <- pSomeone ["but"]
                w2 <- pAny
                return ([w1, w2], Just a1, Just a2)
                )
            <|> try ( do
                a1 <- pSomeone ["misses"]
                w1 <- pString "misses the"
                space
                a2 <- pTillChars "!"
                return ([w1, texcl], Just a1, Just a2)
                )
            <|> ( do
                a1 <- pSomeone ["blocks"]
                w1 <- pString "blocks The flying"
                w2 <- pAny
                return ([w1<>w2, T.empty], Just a1, Nothing)
                )
        return $ newLogEntryData & tag .~ t
                & ac1 ?~ maybe Nobody Creature a1'
                & ac2 ?~ maybe Nobody Creature a2'
                & strs .~ (w1' : w2s')
        )
    <|> ( do
        w1 <- pString "The flying "
        w2 <- pSomething ["misses"]
        a1 <- pTillChars "!"
        return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature a1
                & strs .~ [w1<>w2, texcl]
        )
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
                w2 <- pTillChars ".!"
                -- w2 <- pack . (:[]) <$> (char '!' <|> char '.')
                return ([w1<>w2<>texcl, T.empty], Just someoneA, Nothing)
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
pLogEntryData t@LEBattleEvent2 = do
    --w1 <- fromMaybe "" <$> optionMaybe (pString "The ")
    w1 <- pString "The"
    space
    (a1, a2, w2, w3) <- try ( do
            a1' <- pSomeone [ "is", "counterstrikes!", "slams", "pulls", "loses"
                            , "latches", "skids", "attack" ]
            w2' <- try (pString "is propelled away")
                <|> try (pString "is knocked over")
                <|> try (pString "counterstrikes")
                <|> try (pString "slams into an obstacle")
                <|> try (pString "slams into the")
                <|> try (pString "pulls out and drops the")
                <|> try (pString "loses hold of the")
                <|> try (pString "latches on firmly")
                <|> try (pString "skids along the ground through the")
                <|> pString "attack is interrupted"
            w2'' <- pAny
            return (Creature a1', Nobody, w2'<>w2'', "")
        )
        <|> try ( do
            a1' <- pSomeone [ "lets" ]
            w2' <- pString "lets the "
            w2'' <- pSomething ["drop"]
            w2''' <- pString "drop away as "
            w2'''' <- pAny
            return (Creature a1', Nobody, w2'<>w2''<>ts<>w2'''<>w2'''', "")
        )
        <|> try ( do
            a1' <- pSomeone [ "bends" ]
            w2' <- pString "bends "
            w2'' <- pSomething ["and"]
            w2''' <- pString "bends "
            w2'''' <- pSomething ["colapses!"]
            w2''''' <- pString "colapses!"
            return (Creature a1', Nobody, 
                w2'<>w2''<>ts<>w2'''<>w2''''<>ts<>w2''''', "")
        )
        <|> try ( do
            a1' <- pSomeone [ "cancels" ]
            w2' <- pString "cancels "
            w2'' <- pTillChars ":" 
            w2''' <- try (pString " Too injured.") <|> pString " Webbed."
            return (Creature a1', Nobody, w2'<>w2''<>tc<>w2''', "")
        )
        <|> try ( do
            a1' <- pSomeone [ "manages", "grabs", "locks"
                            , "adjusts", "releases", "breaks", "is", "places"
                            , "struggles", "throws", "shakes", "takes" ]
            w2' <- try (pString "manages to stop where The")
                <|> try (pString "grabs The")
                <|> try (pString "locks The")
                <|> try (pString "adjusts the grip of The")
                <|> try (pString "releases the grip of The")
                <|> try (pString "breaks the grip of The")
                <|> try (pString "is unable to break the grip of The")
                <|> try (pString "is ripped away and remains in The")
                <|> try (pString "places a chokehold on The")
                <|> try (pString "strangles The")
                <|> try (pString "struggles in vain against the grip of The")
                <|> try (pString "throws The")
                <|> try (pString "shakes The")
                <|> pString "takes The"
            space
            a2' <- pSomeone [ "!", "uses", "by", "with", "on", "from"
                            , "grip", "around", "down" ]
            w3' <- pAny
            return (Creature a1', Creature a2', w2', w3')
        )
        <|> ( do
            a1' <- pSomeone [ "rushes", "leaps" ]
            w2' <- try (pString "rushes by The")
                <|> pString "leaps at The"
            space
            a2' <- pSomeoneNoTrim [ "!" ]
            w3' <- pAny
            return (Creature a1', Creature a2', w2', w3')
        )
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ a1
        & ac2 ?~ a2
        & strs .~ [w1, w2, w3]
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
    m <- pAny
    return $ newLogEntryData & tag .~ t
        & mat ?~ m
pLogEntryData t@LEAnimalBirth =
    try ( do
        m <- pSomeone ["has"]
        w1 <- pString "has given birth to "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & mat ?~ m
            & strs .~ [w1<>w2]
        )
    <|> ( do
        m <- pSomeone ["have"]
        w1 <- pString "have hatched."
        return $ newLogEntryData & tag .~ t
            & mat ?~ m
            & strs .~ [w1]
        )
pLogEntryData t@LEAnimalSlaughtered = do
    optional (string "The ")
    m <- pSomething ["has"]
    string "has been slaughtered."
    return $ newLogEntryData & tag .~ t
        & mat ?~ m
pLogEntryData t@LESomeoneBecome = do
    try ( do
        optional (string "The ")
        dA <- pActor ["has"]
        w1 <- pString "has become a "
        w2 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & strs .~ [w1<>w2]
        )
    <|> try ( do
        try (string "A ") <|> string "An "
        dA <- pActor ["has"]
        w1 <- pString "has become a"
        space
        m <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & mat ?~ m
            & strs .~ [w1]
        )
    <|> try ( do
        dA <- pActor ["has"]
        w1 <- pString "has been re-elected."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & strs .~ [w1]
        )
    <|> try ( do
        dA <- pActor ["has"]
        w1 <- pString "has been elected mayor."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & strs .~ [w1]
        )
    <|> try ( do
        dA <- pActor ["became"]
        w1 <- pString "became "
        w2 <- try (pString "mayor.") <|> pString "expedition leader."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & strs .~ [w1<>w2]
        )
    <|> try ( do
        optional (try (string "A ") <|> string "An ")
        dA <- pActor ["has"]
        w1 <- pString "has grown to become a"
        space
        m <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & mat ?~ m
            & strs .~ [w1]
        )
    <|> try ( do
        optional (string "The ")
        dA <- pActor []
        w1 <- pString ", being the rightful heir, has inherited the position of "
        w2 <- try (pString "king") <|> pString "queen"
        w3 <- pString " of The"
        space
        m <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & mat ?~ m
            & strs .~ [w1<>w2<>w3]
        )
    <|> try ( do
        w1 <- pString "Mayor position is now vacant."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1]
        )
    <|> try ( do
        w1 <- pString "Expedition leader was replaced by mayor."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1]
        )
    <|> ( do
        w1 <- pString "Expedition leader position is now vacant."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1]
        )
pLogEntryData t@LEMandate = do
    optional (string "The ")
    dA <- pActor ["has"]
    w1 <- pString "has"
    space
    w2 <- try (pString "mandated the construction of certain goods.")
        <|> try (pString "imposed a ban on certain exports.")
        <|> try (pString "ended a mandate.")
        <|> try (pString "a new demand.")
        <|> pString "forgotten a demand."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ dA
        & strs .~ [w1<>ts<>w2]
pLogEntryData t@LETrade = do
    try ( do
        w1 <- pString "A"
        space
        w2Mb <- optionMaybe (
            try (pString "human") 
            <|> try (pString "elven") 
            <|> try (pString "dwarven")
            <|> pString "goblin"
            )
        spaces
        w3 <- pString "caravan from "
        w4 <- pSomething ["has"]
        w5 <- pString "has arrived."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>ts<>fromMaybe "" w2Mb<>ts<>w3<>w4<>ts<>w5]
        )
    <|> try ( do
        w1 <- pString "Merchants have arrived and are unloading their goods."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1]
        )
    <|> try ( do
        w1 <- pString "Their wagons have bypassed your inaccessible site."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1]
        )
    <|> try ( do
        w1 <- pString "No outpost liaison? How curious..."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1]
        )
    <|> try ( do
        w1 <- pString "The merchants "
        w2 <- try (pString "from ") <|> pString "need "
        w3 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2<>w3<>tp]
        )
    <|> try ( do
        w1 <- pSomething ["cancels"]
        w2 <- pString "cancels Trade at Depot: "
        w3 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2<>w3<>tp]
        )
pLogEntryData t@LEVisit = do
    try ( do
        acA <- pActor ["is"]
        w1 <- pSomething ["is"]
        w2 <- pString "is visiting"
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & strs .~ ["",if T.null w1 then w2<>tp else w1<>ts<>w2<>tp] 
        )
    <|> try ( do
        w1 <- pString "The"
        space
        acA <- pActor ["from"]
        w2 <- pSomething ["has"]
        w3 <- pString "has arrived."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & strs .~ [w1, w2<>ts<>w3] 
        )
    <|> ( do
        w1 <- pString "A"
        space
        acA <- pActor ["diplomat"]
        w2 <- pString "diplomat from "
        w3 <- pSomething ["has"]
        w4 <- pString "has arrived."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & strs .~ [w1, w2<>w3<>ts<>w4] 
        )
pLogEntryData t@LESting = do
    optional (string "The ")
    acA <- pActor ["has", "have"]
    w1 <- try (pString "has") <|> pString "have"
    space
    w2 <- pString "been stung by a "
    w3 <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1<>ts<>w2<>w3<>texcl] 
pLogEntryData t@LEItem = do
    optional (string "The ")
    acA <- pActor ["has"]
    w1 <- pString "has grown attached to a "
    w2 <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1<>w2<>texcl] 
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
pLogEntryData t@LEFishing = do
    w1 <- pString "There is nothing to catch in the "
    w2 <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2<>tp]
pLogEntryData t@LEAdoption = do
    sA <- pSomeone ["has"]
    w1 <- pString "has adopted"
    space
    dB <- pActor []
    char '.'
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature sA
        & ac2 ?~ dB
        & strs .~ [w1]
pLogEntryData t@LESkillLevel = do
    try ( do 
        dA <- pActor ["is"]
        w1 <- try (pString "is now ") <|> pString "is no longer "
        w2Mb <- optionMaybe (pString "very ")
        w3 <- pString "rusty"
        space
        m <- pTillChars "."
        char '.'
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & mat ?~ m
            & strs .~ [w1<>fromMaybe "" w2Mb<>w3]
        )
    <|> try ( do
        dA <- pActor ["has"]
        w1 <- pString "has became "
        w2 <- try (pString "Proficient") 
            <|> try (pString "Accomplished")
            <|> pString "Legendary"
        space
        m <- pTillChars "."
        char '.'
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & mat ?~ m
            & strs .~ [w1<>w2]
        )
    <|> ( do
        dA <- pActor []
        w1 <- pString ": "
        w2 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ dA
            & strs .~ [w1<>w2<>". That was very satisfying!"]
        )
pLogEntryData t@LEMoodNormal = do
    dA <- pActor ["cancels", "is", "withdraws", "begins", "has"]
    try ( do 
            w1 <- pTillChars ":"
            w2 <- pString ": Taken by mood."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1<>w2, ""]
            )
        <|> try ( do 
            w1 <- pString "is taken by a fey mood!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "withdraws from society..."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "begins to stalk and brood..."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "has begun a mysterious construction!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "has claimed a"
            space
            m <- pTillChars "."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & mat ?~ m
                & strs .~ [w1, tp]
            )
        <|> ( do 
            w1 <- pString "has created "
            w2 <- pTillChars ","
            w3 <- pString " a"
            m <- pTillChars "!"
            w4 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & mat ?~ m
                & strs .~ [w1<>w2<>tc<>w3, "! "<>w4]
            )
pLogEntryData t@LEMoodInsane = do
    dA <- pActor ["has", "looses", "cancels", "is"]
    try ( do 
            w1 <- pString "has been possessed!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pString "looses a roaring laughter, fell and terrible!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pTillChars ":"
            w2 <- pString ": Went insane."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1<>w2]
            )
        <|> try ( do 
            w1 <- pString "has gone berserk!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pString "has gone stark raving mad!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pString "is stricken by melancholy!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ dA
                & strs .~ [w1]
            )
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
pLogEntryData t@LEGuild =
    try ( do
        w1 <- pString "The "
        w2 <- pSomething ["guild"]
        w3 <- pString "guild, has been established."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2<>ts<>w3]
        )
    <|> ( do
        w1 <- pString "The guildhall agreement with "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2]
        )
pLogEntryData t@LEBattleBreath = do
    --w1 <- fromMaybe "" <$> optionMaybe (pString "The ")
    w1 <- pString "The"
    space
    try ( do
            a1 <- pSomeone [ "breathes", "hurls", "is", "blockes", "shoots" ]
            w2 <- try (pString "breathes a")
                <|> try (pString "breathes fire")
                <|> try (pString "hurls a ball")
                <|> try (pString "is caught up in the web")
                <|> try (pString "is partially free of the web")
                <|> try (pString "is completely free of the web")
                <|> try (pString "blocks the breath")
                <|> try (pString "blocks the fire")
                <|> pString "shoots out thick strands of webbing"
            w3 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature a1
                & strs .~ [w1, w2<>w3]
            )
        <|> ( do
            a1 <- pSomeone [ "is" ]
            w2 <- pString "is caught in a "
            w3 <- try (pString "cloud of ") <|> pString "burst of "
            w4 <- pSomething [ "extract" ]
            w5 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature a1
                & strs .~ [w1, w2<>w3<>w4<>ts<>w5]
            )
pLogEntryData t@LEMasterpieceLost = do
    w1 <- pString "A masterwork of "
    w2 <- pSomething ["has"]
    w3 <- pString "has been lost!"
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2<>ts<>w3]
pLogEntryData t@LEHazard = do
    w1 <- pString "The "
    a1 <- pActor ["is"]
    w2 <- pString "is caught in a "
    w3 <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ a1
        & strs .~ [w2<>w3]
pLogEntryData t@LEMiningWarning = do
    try ( do
        w1 <- pString "Digging designation cancelled: "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2] 
        )
    <|> try ( do
        w1 <- pString "Raw adamantine!  Praise the miners!"
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1] 
        )
    <|> try ( do
        w1 <- pString "You have discovered an expansive cavern deep underground."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1] 
        )
    <|> try ( do
        w1 <- pString "You have discovered an eerie cavern."
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2] 
        )
    <|> try ( do
        w1 <- pString "Horrifying screams come from the darkness below!"
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1] 
        )
    <|> try ( do
        w1 <- pString "You have discovered a "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2] 
        )
    <|> try ( do
        w1 <- pString "A section of the cavern has collapsed!"
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1] 
        )
    <|> ( do
        w1 <- pString "Something has collapsed on the surface!"
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1] 
        )


-- *****************************************************************************

-- | Base parsing rule; place move specific and more friquent rules to top
baseRule :: Parsec Text LogParseConfig LogEntryData
baseRule = 
    try (pLogEntryData LEJobSuspension)     -- needs to be on top of LECraftCancel and LEJobCancel
    <|> try (pLogEntryData LECraftCancel)   -- needs to be beetwean of LEJobSuspension and LEJobCancel
    <|> try (pLogEntryData LEJobCancel)     -- needs to be on bottom of of LEJobSuspension and LECraftCancel
    <|> try (pLogEntryData LEProductionCompleted)
    <|> try (pLogEntryData LEMasterpieceImproved)
    <|> try (pLogEntryData LEMasterpieceCreated)
    <|> try (pLogEntryData LEDeathFound)
    <|> try (pLogEntryData LECrimeTheft)
    <|> try (pLogEntryData LEDFHackAutomation)
    <|> try (pLogEntryData LEMiningStruck)
    <|> try (pLogEntryData LEBattleMiss)
    <|> try (pLogEntryData LEBattleEvent)
    <|> try (pLogEntryData LEBattleStrike)
    <|> try (pLogEntryData LEBattleHit)
    <|> try (pLogEntryData LEBattleEvade)
    <|> try (pLogEntryData LEBattleStatus)
    <|> try (pLogEntryData LEBattleEvent2)
    <|> try (pLogEntryData LEGore)
    <|> try (pLogEntryData LEAnimalGrown)
    <|> try (pLogEntryData LEAnimalBirth)
    <|> try (pLogEntryData LEAnimalSlaughtered)
    <|> try (pLogEntryData LESomeoneBecome)
    <|> try (pLogEntryData LEMandate)
    <|> try (pLogEntryData LETrade)
    <|> try (pLogEntryData LEVisit)
    <|> try (pLogEntryData LESting)
    <|> try (pLogEntryData LEItem)
    <|> try (pLogEntryData LEWeather)
    <|> try (pLogEntryData LEFishing)
    <|> try (pLogEntryData LEAdoption)
    <|> try (pLogEntryData LESkillLevel)
    <|> try (pLogEntryData LEMoodNormal)
    <|> try (pLogEntryData LEMoodInsane)
    <|> try (pLogEntryData LESeason)
    <|> try (pLogEntryData LESystem)
    <|> try (pLogEntryData LEGuild)
    <|> try (pLogEntryData LEBattleBreath)  -- must be on top of LEHazard
    <|> try (pLogEntryData LEMasterpieceLost)
    <|> try (pLogEntryData LEHazard)        -- must be on bottom of LEBattleBreath
    <|> try (pLogEntryData LEMiningWarning)
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

