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
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module LogParser.Rules where

import Control.Exception ( throw )
import Control.Lens ( (?~), (.~), (&) )
import Data.Text ( pack, Text )
import qualified Data.Text as T
import Text.Parsec
import Data.Maybe

import LogParser.LogEntry
import LogParser.Rules.Helpers


data LogEntryStructure =  ActorFirst Text Actor | OtherLogEntry

-- | Parsing rules for each LogEntryTag constructor
pLogEntryData :: LogEntryStructure -> LogEntryTag -> Parsec Text LogParseConfig LogEntryData
pLogEntryData _ LEDefault = do
    w <- pMany1 anyChar
    --parserTrace "label1"
    return $ newLogEntryData & strs .~ [w] 
pLogEntryData (ActorFirst _ acA)    t@LEJobSuspension = do
    try ( do
        string "cancels "
        j <- pString "Construct Building"
        string ": "
        w1 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & job ?~ j
            & strs .~ [T.empty, w1, tcol] 
        )
    <|> ( do
        string "cancels "
        j <- pString "Link a Building to Trigger"
        string ": "
        w1 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & job ?~ j
            & strs .~ [T.empty, w1, tcol] 
        )
pLogEntryData OtherLogEntry         t@LEJobSuspension = do
    try ( do
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
pLogEntryData (ActorFirst _ acA)    t@LECraftCancel = do
    string "cancels "
    j <- pTillChars ":"
    space
    string "Needs "
    m <- pTillChars "." 
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ j
        & mat ?~ m
pLogEntryData OtherLogEntry         t@LECraftCancel = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEJobCancel = do
    string "cancels "
    j <- pMany1 (noneOf [':'])
    string ": "
    notFollowedBy (try (string "Throwing"))
    wA <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & job ?~ j
        & strs .~ ["cancels",":", wA] 
pLogEntryData OtherLogEntry         t@LEJobCancel = do
    w1 <- pString "Jobs removed from unpowered Magma "
    w2 <- pAny
    return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2, "", ""] 
pLogEntryData _ t@LEProductionCompleted = do
    j <- pack <$> manyTill anyChar
            (try (lookAhead (string " (" >> many1 digit >> char ')')))
    space
    char '('
    m <- pMany1 digit
    char ')'
    string " has been completed."
    return $ newLogEntryData & tag .~ t
        & job ?~ j
        & mat ?~ m
pLogEntryData (ActorFirst _ acA)    t@LEMasterpieceImproved = do
    w1 <- pString "has improved"
    w2 <- option "" (try (pString " a"))
    spaces
    m <- pSomething ["masterfully"]
    w3 <- pString "masterfully!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & mat ?~ m
        & strs .~ [w1<>w2, w3] 
pLogEntryData OtherLogEntry         t@LEMasterpieceImproved = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEMasterpieceCreated = do
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
pLogEntryData OtherLogEntry         t@LEMasterpieceCreated = fail ""
pLogEntryData _ t@LECrimeTheft = do
    matS <- pFullName
    wA <- pString "is missing from its proper place!"
    return $ newLogEntryData & tag .~ t
        & mat ?~ matS
        & strs .~ [wA] 
pLogEntryData _ t@LEDFHackAutomation = do
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
pLogEntryData _ t@LEMiningStruck = do
    w1 <- pString "You have struck"
    space
    m <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & mat ?~ m
        & strs .~ [w1] 
pLogEntryData (ActorFirst w1 acA)   t@LEBattleMiss = do
    --w1' <- option "" (try (pString "The"))
    --spaces
    (w2s, acB) <- try ( do
            --a1 <- pSomeone ["attacks", "strikes"]
            w1' <- try (pString "attacks the") <|> pString "strikes at the"
            space
            a2 <- pSomeone ["but"]
            w2' <- pAny
            return ([w1', w2'], Creature a2)
            )
        <|> try ( do
            --a1 <- pSomeone ["misses"]
            w1' <- pString "misses the"
            space
            a2 <- pTillChars "!"
            return ([w1', texcl], Creature a2)
            )
        <|> ( do
            --a1 <- pSomeone ["blocks"]
            w1' <- pString "blocks The flying"
            w2' <- pAny
            return ([w1'<>w2', T.empty], Nobody)
            )
    return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & ac2 ?~ acB
            & strs .~ (w1 : w2s)
pLogEntryData OtherLogEntry         t@LEBattleMiss = do
    w1 <- pString "The flying "
    w2 <- pSomething ["misses"]
    acA <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
            & ac1 ?~ Creature acA
            & strs .~ [w1<>w2, texcl]
pLogEntryData (ActorFirst w1 acA)   t@LEBattleEvent = do
    try ( do
        -- w1' <- pString "The"
        -- spaces
        (w2s, acB) <- try (do 
                -- someoneA <- pSomeone ["charges", "collides"]
                w1' <- try (pString "charges at") <|> pString "collides with"
                string " the "
                someoneB <- pTillChars "!"
                return ([w1', texcl], Creature someoneB)
                )
            <|> try ( do
                -- someoneA <- pSomeone ["has", "is", "stands", "passes", "falls", "regains"]
                w1' <- try (pString "has been stunned")            
                    <|> try (pString "is knocked over")
                    <|> try (pString "has been knocked unconscious") 
                    <|> try (pString "stands up")  
                    <|> try (pString "passes out")                   
                    <|> try (pString "falls over")
                    <|> try (pString "regains consciousness")        
                    <|> pString "is no longer stunned"
                w2' <- pTillChars ".!"
                return ([w1'<>w2'<>texcl, T.empty], Nobody)
                )
            <|> try ( do
                -- someoneA <- pSomeone ["bounces"]
                w1' <- pString "bounces backward!"
                return ([w1', T.empty], Nobody)
                )
            <|> try ( do
                -- someoneA <- pSomeone ["collapses"]
                w1' <- pString "collapses and falls to the ground from "
                w2' <- pAny
                return ([w1'<>w2', T.empty], Nobody)
                )
            <|> ( do
                -- someoneA <- pSomeone ["looks"]
                w1' <- pString "looks surprised by the ferocity of"
                w2' <- option "" (try (pString "The "))
                someoneB <- pSomeone ["onslaught"]
                w3' <- pString "onslaught!"
                return ([w1'<>ts<>w2', w3'], Creature someoneB)
                )
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & ac2 ?~ acB
            & strs .~ (w1 : w2s)
        )
pLogEntryData OtherLogEntry t@LEBattleEvent = do
    w1 <- pString "They tangle together and "
    w2 <- pAny
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2]
pLogEntryData (ActorFirst _ acA)    t@LEBattleStrike = do
    -- string "The "
    -- someoneA <- pSomeone 
        -- ["leaps","punches","punches","catches","snatches","stabs"
        -- ,"grabs","hacks","pushes","misses","slashes","shakes"
        -- ,"blocks","gores","strangles","strikes","scratches","kicks"
        -- ,"attacks","lashes","slaps","bashes","bites","strikes"
        -- ,"punches","releases","throws","takes","locks","bends"
        -- ,"places","gouges"]
    wA <- try (pString "leaps at")   <|> try (pString "punches")
        <|> try (pString "catches")  <|> try (pString "snatches at") <|> try (pString "stabs")
        <|> try (pString "grabs")    <|> try (pString "hacks")    <|> try (pString "pushes")
        <|> try (pString "misses")   <|> try (pString "slashes")  <|> try (pString "shakes")
        <|> try (pString "blocks")   <|> try (pString "gores")    <|> try (pString "strangles")
        <|> try (pString "strikes")  <|> try (pString "scratches")<|> try (pString "kicks")
        <|> try (pString "attacks")  <|> try (pString "lashes")   <|> try (pString "slaps")
        <|> try (pString "bashes")   <|> try (pString "bites")    <|> try (pString "strikes at")
        <|> try (pString "releases") <|> try (pString "throws")
        <|> try (pString "takes")    <|> try (pString "locks")    <|> try (pString "bends")
        <|> try (pString "places a chokehold on")                 <|> pString "gouges"
    string " the "
    someoneB <- pSomeone ["in"]
    wB <- pAny
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & ac2 ?~ Creature someoneB
        & strs .~ [wA, wB]
pLogEntryData OtherLogEntry         t@LEBattleStrike = fail ""
pLogEntryData _ t@LEBattleHit = do
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
        <|> try ( do
            w1' <- pString "flying "
            w2' <- pSomething ["strikes"]
            w3' <- pAny
            return [w1', w2', w3']
            )
        <|> try ( do
            w1' <- pSomething ["twists"]
            w2' <- pString "twists the "
            w3' <- pTillChars "!"
            return [w1'<>ts<>w2'<>w3'<>texcl]
            )
        <|> ( do
            w1' <- pSomething ["bends"]
            w2' <- pString "bends the "
            w3' <- pTillChars "!"
            return [w1'<>ts<>w2'<>w3'<>texcl]
            )
    return $ newLogEntryData & tag .~ t
        & strs .~ [T.concat (w1'':w2s'')]
pLogEntryData (ActorFirst w1 acA)   t@LEBattleEvade = do
    try ( do
        (w1s', a1', a2') <- try ( do
                w2 <- try (pString "jumps away")
                    <|> try (pString "jumps out")
                    <|> try (pString "jump away")
                    <|> try (pString "scrambles out of")
                    <|> try (pString "scrambles away")
                    <|> try (pString "rolls out of")
                    <|> try (pString "rolls away")
                    <|> try (pString "bats The")
                    <|> pString "falls over"
                w3 <- pAny
                return ([w2<>w3, T.empty], acA, Nothing)
                )
            <|> try ( do 
                w2 <- try (pString "blocks")
                    <|> pString "miss"
                w3 <- pAny
                return ([w2<>w3, T.empty], acA, Nothing)
                )
            <|> ( do 
                w2 <- pString "strikes at"
                acB <- pActor ["but"]
                w3 <- pString "but the shot is blocked"
                w4 <- pAny
                return ([w2, w3<>w4], acA, Just acB)
                )
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ a1'
            & ac2 .~ (Just =<< a2')
            & strs .~ w1:w1s'
        )
pLogEntryData OtherLogEntry         t@LEBattleEvade = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEBattleStatus = do
    w2s <- try ( do
            -- optional (try (string "The "))
            -- dA <- pSomeone [ "skids", "has", "is", "gives", "passes", "looks"
            --             , "vomits", "retches", "regains" ]
            w2' <- try (pString "skids along the ground!")
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
            w3' <- pAny
            return [w2', w3']
            )
        <|> try ( do
            -- optional (try (string "The "))
            -- dA <- pSomeone ["cancels"]
            w2' <- pString "cancels"
            w3' <- pTillChars ":"
            w4' <- try (pString " Paralyzed.") 
                <|> pString " Resting injury."
            return [w2', w3', w4']
            )
        <|> ( do
            -- w1' <- pack . (:[]) <$> oneOf "tT"
            -- w2' <- pString "he "
                    -- dA <- pSomeone ["vomits", "retches", "looks", "trouble"]
            w2' <- try (pString "vomits")
                <|> try (pString "retches")
                <|> try (pString "looks sick")
                <|> try (pString "looks even more sick")
                <|> try (pString "trouble breathing")
            w3' <- pAny
            return [w2', w3']
            )
        <|> try ( do
            -- dA <- pSomeone ["skips", "rolls", "unrolls"]
            w2' <- try (pString "skips across the water!")
                <|> try (pString "rolls into a ball.")
                <|> pString "unrolls."
            return [w2']
        )
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [T.concat w2s]
pLogEntryData OtherLogEntry         t@LEBattleStatus = do
    w1 <- pString "The vomit "
    w2 <- try (pString "disappears into the water.")
        <|> try (pString "burns away in the lava.")
        <|> pString "splatters into the shallow puddle of water."
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2]
pLogEntryData (ActorFirst w1 acA)   t@LEBattleEvent2 = do
    -- w1 <- pString "The"
    -- space
    (acB, w2, w3) <- try ( do
            -- a1' <- pSomeone [ "is", "counterstrikes!", "slams", "pulls", "loses"
            --                 , "latches", "skids", "attack" ]
            w2' <- try (pString "is propelled away")
                <|> try (pString "is knocked over")
                <|> try (pString "is ripped to")
                <|> try (pString "counterstrikes")
                <|> try (pString "slams into an obstacle")
                <|> try (pString "slams into the")
                <|> try (pString "pulls out and drops the")
                <|> try (pString "loses hold of the")
                <|> try (pString "latches on firmly")
                <|> try (pString "skids along the ground through the")
                <|> pString "attack is interrupted"
            w2'' <- pAny
            return (Nobody, w2'<>w2'', "")
        )
        <|> try ( do
            -- a1' <- pSomeone [ "lets" ]
            w2' <- pString "lets the "
            w2'' <- pSomething ["drop"]
            w2''' <- pString "drop away as "
            w2'''' <- pAny
            return (Nobody, w2'<>w2''<>ts<>w2'''<>w2'''', "")
        )
        <|> try ( do
            -- a1' <- pSomeone [ "bends" ]
            w2' <- pString "bends "
            w2'' <- pSomething ["and"]
            w2''' <- pString "bends "
            w2'''' <- pSomething ["colapses!"]
            w2''''' <- pString "colapses!"
            return (Nobody, 
                w2'<>w2''<>ts<>w2'''<>w2''''<>ts<>w2''''', "")
        )
        <|> try ( do
            -- a1' <- pSomeone [ "cancels" ]
            w2' <- pString "cancels "
            w2'' <- pTillChars ":" 
            w2''' <- try (pString " Too injured.") <|> pString " Webbed."
            return (Nobody, w2'<>w2''<>tc<>w2''', "")
        )
        <|> try ( do
            -- a1' <- pSomeone [ "locks", "releases", "is", "places", "strangles" ]
            w2' <- try (pString "locks ")
                <|> try (pString "releases the grip of ")
                <|> try (pString "releases the joint lock of ")
                <|> try (pString "is unable to break the grip of ")
                <|> try (pString "is ripped away and remains in ")
                <|> try (pString "places a chokehold on ")
                <|> pString "strangles "
            w2'' <- try (pString "the") <|> pString "The"
            space
            a2' <- pSomeoneWithEnd "'s"
            space
            w3' <- pAny
            return (Creature a2', w2'<>w2'', w3')
        )
        <|> try ( do
            -- a1' <- pSomeone [ "manages", "grabs"
            --                 , "adjusts", "releases", "breaks"
            --                 , "struggles", "throws", "shakes", "takes" ]
            w2' <- try (pString "manages to stop where ")
                <|> try (pString "grabs ")
                <|> try (pString "adjusts the grip of ")
                <|> try (pString "releases the grip of ")
                <|> try (pString "breaks the grip of ")
                <|> try (pString "struggles in vain against the grip of ")
                <|> try (pString "throws ")
                <|> try (pString "shakes ")
                <|> pString "takes "
            w2'' <- try (pString "the") <|> pString "The"
            space
            a2' <- pSomeone [ "uses", "by", "with", "on", "from"
                            , "grip", "around", "down" ]
            w3' <- pAny
            return (Creature a2', w2'<>w2'', w3')
        )
        <|> ( do
            -- a1' <- pSomeone [ "rushes", "leaps" ]
            w2' <- try (pString "rushes by ")
                <|> pString "leaps at "
            w2'' <- try (pString "the") <|> pString "The"
            space
            a2' <- pSomeoneNoTrim [ "!" ]
            w3' <- pAny
            return (Creature a2', w2'<>w2'', w3')
        )
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & ac2 ?~ acB
        & strs .~ [w1, w2, w3]
pLogEntryData OtherLogEntry         t@LEBattleEvent2 = fail ""
pLogEntryData (ActorFirst w1 acA)   t@LEBattleTrance = do
    w2 <- try (pString "has entered a martial trance!") 
        <|> pString "has left the martial trance."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1, w2]
pLogEntryData OtherLogEntry         t@LEBattleTrance = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEEmotion = do
    try ( do
            w1 <- pString ": This is a fight!"
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
        <|> try ( do
            w1 <- pString ": Has the tide turned?"
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
        <|> try ( do
            w1 <- pString ": Can it all end so quickly?"
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
        <|> try ( do
            w1 <- pString ": I must withdraw!"
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
        <|> try ( do
            w1 <- pString ": Help!"
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
        <|> try ( do
            w1 <- pString ": The battle rages..."
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
        <|> ( do
            w1 <- pString ": I cannot just stand by."
            spaces
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>ts<>w2]
            )
pLogEntryData OtherLogEntry         t@LEEmotion = fail ""
pLogEntryData _ t@LEGore = do
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
            w1 <- pString "Many nerves have been severed"
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
pLogEntryData _ t@LEAnimalGrown = do
    string "An animal has grown to become a "
    m <- pAny
    return $ newLogEntryData & tag .~ t
        & mat ?~ m
pLogEntryData _ t@LEAnimalBirth =
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
pLogEntryData _ t@LEAnimalSlaughtered = do
    optional (try (string "The "))
    m <- pSomething ["has"]
    string "has been slaughtered."
    return $ newLogEntryData & tag .~ t
        & mat ?~ m
pLogEntryData _ t@LEAnimalTraining = do
    w0 <- option "" (try (pString "The"))
    spaces
    try ( do
            acA <- pSomeone [ "has" ]
            w1 <- try (pString "has reverted to a wild state!")
                <|> try (pString "has forgotten her training!")
                <|> pString "has forgotten his training!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature acA
                & strs .~ [w0, w1]
            )
        <|> ( do
            w1 <- pSomething [ "of" ]
            w2 <- pString "of The"
            space
            w3 <- pSomething [ "now", "have", "are" ]
            w4 <- try (pString "now know a few facts about")
                <|> try (pString "have attained a general familiarity with")
                <|> pString "are now quite knowledgeable"
            w5 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w0<>ts<>w1<>ts<>w2<>ts<>w3<>ts<>w4<>w5, ""]
            )
pLogEntryData _ t@LEAnimalMisc = do
    w0 <- option "" (try (pString "The"))
    spaces
    try ( do
            acA <- pSomeone [ "has" ]
            w1 <- try (pString "has gnawed its way out of confinement!")
                <|> pString "has guzzled some"
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature acA
                & strs .~ [w0, w1<>w2]
            )
pLogEntryData (ActorFirst w0 acA)   t@LESocial = do
    try ( do
            w1 <- try (pString "has organized a party at")
                <|> pString "has given birth to"
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w0, w1<>w2]
            )
        <|> ( do
            w1 <- pString "and the"
            acB <- pActor [ "have" ]
            w2 <- pString "have married.  Congratulations!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & ac2 ?~ acB
                & strs .~ [w0, w1, w2]
            )
pLogEntryData OtherLogEntry         t@LESocial = do
    w1 <- pString "They have "
    w2 <- try (pString "organized a wedding reception at")
        <|> pString "decided to forego any formal celebrations"
    w3 <- pAny
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2<>w3, "", ""]
pLogEntryData (ActorFirst _ acA)    t@LESomeoneBecome = do
    try ( do
            w1 <- pString "has become a "
            w2 <- pTillChars "."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>w2<>tp]
            )
        <|> try ( do
            w1 <- pString "has been re-elected."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> try ( do
            w1 <- pString "is more experienced."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> try ( do
            w1 <- pString "has been elected mayor."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> ( do
            w1 <- pString "became "
            w2 <- try (pString "mayor.") <|> pString "expedition leader."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>w2]
            )
pLogEntryData OtherLogEntry         t@LESomeoneBecome = do
    try ( do
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
            optional (try (string "The "))
            dA <- pTillChars ","
            w1 <- pString " being the rightful heir, has inherited the position of "
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature dA
                & strs .~ [tc<>w1<>w2]
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
pLogEntryData (ActorFirst _ acA)    t@LEMandate = do
    w1 <- pString "has"
    space
    w2 <- try (pString "mandated the construction of certain goods.")
        <|> try (pString "imposed a ban on certain exports.")
        <|> try (pString "ended a mandate.")
        <|> try (pString "a new demand.")
        <|> pString "forgotten a demand."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1<>ts<>w2]
pLogEntryData OtherLogEntry         t@LEMandate = do
    dA <- pSomeoneWithEnd "'s"
    space
    w1 <- try (pString "mandate has ended.")
        <|> pString "mandates have ended."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature dA
        & strs .~ [w1]
pLogEntryData (ActorFirst _ acA)    t@LETrade = do
    w1 <- pString "has altered the prices of goods."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1]
pLogEntryData OtherLogEntry         t@LETrade = do
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
    <|> ( do
        w1 <- pSomething [ "cancels" ]
        w2 <- pString "cancels Trade at Depot: "
        w3 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2<>w3<>tp]
        )
pLogEntryData (ActorFirst w1 acA)   t@LEVisit = do
    try ( do
        w2 <- pString "is visiting."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & strs .~ ["", w2] 
        )
    <|> ( do
        w2 <- pSomething ["has"]
        w3 <- pString "has arrived."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & strs .~ [w1, w2<>ts<>w3] 
        )
pLogEntryData OtherLogEntry         t@LEVisit = do
    w1 <- pString "A"
    space
    acA <- pActor ["diplomat"]
    w2 <- pString "diplomat from "
    w3 <- pSomething ["has"]
    w4 <- pString "has arrived."
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1, w2<>w3<>ts<>w4] 
pLogEntryData (ActorFirst _ acA)    t@LESting = do
    w1 <- try (pString "has") <|> pString "have"
    space
    w2 <- pString "been stung by a "
    w3 <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1<>ts<>w2<>w3<>texcl] 
pLogEntryData OtherLogEntry         t@LESting = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEItem = do
    try ( do
            w1 <- pString "has grown attached to a"
            space
            m <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & mat ?~ m
                & strs .~ [w1] 
            )
        <|> ( do
            w1 <- pString "has bestowed "
            w2 <- pSomething [ "upon" ]
            w3 <- pString "upon a"
            space
            m <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & mat ?~ m
                & strs .~ [w1<>w2<>ts<>w3]
            )
pLogEntryData OtherLogEntry         t@LEItem = fail ""
pLogEntryData _ t@LEWeather = do
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
pLogEntryData _ t@LEFishing = do
    w1 <- pString "There is nothing to catch in the "
    w2 <- pTillChars "."
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2<>tp]
pLogEntryData _ t@LEAdoption = do
    sA <- pSomeone ["has"]
    w1 <- pString "has adopted"
    space
    dB <- pActor []
    char '.'
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature sA
        & ac2 ?~ dB
        & strs .~ [w1]
pLogEntryData (ActorFirst _ acA)    t@LESkillLevel = do
    try ( do 
        w1 <- try (pString "is now ") <|> pString "is no longer "
        w2Mb <- optionMaybe (pString "very ")
        w3 <- pString "rusty"
        space
        m <- pTillChars "."
        char '.'
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & mat ?~ m
            & strs .~ [w1<>fromMaybe "" w2Mb<>w3]
        )
    <|> try ( do
        w1 <- pString "has became "
        w2 <- try (pString "Proficient") 
            <|> try (pString "Accomplished")
            <|> pString "Legendary"
        space
        m <- pTillChars "."
        char '.'
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & mat ?~ m
            & strs .~ [w1<>w2]
        )
    <|> ( do
        w1 <- pString ": "
        w2 <- pTillChars "."
        return $ newLogEntryData & tag .~ t
            & ac1 ?~ acA
            & strs .~ [w1<>w2<>". That was very satisfying!"]
        )
pLogEntryData OtherLogEntry         t@LESkillLevel = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEMoodNormal = do
    try ( do 
            w1 <- pTillChars ":"
            w2 <- pString ": Taken by mood."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>w2, ""]
            )
        <|> try ( do 
            w1 <- pString "is taken by a fey mood!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "withdraws from society..."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "begins to stalk and brood..."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "has begun a mysterious construction!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1, ""]
            )
        <|> try ( do 
            w1 <- pString "has claimed a"
            space
            m <- pTillChars "."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
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
                & ac1 ?~ acA
                & mat ?~ m
                & strs .~ [w1<>w2<>tc<>w3, "! "<>w4]
            )
pLogEntryData OtherLogEntry         t@LEMoodNormal = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEMoodInsane = do
    try ( do 
            w1 <- pString "has been possessed!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pString "looses a roaring laughter, fell and terrible!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pTillChars ":"
            w2 <- pString ": Went insane."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>w2]
            )
        <|> try ( do 
            w1 <- pString "has gone berserk!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> try ( do 
            w1 <- pString "has gone stark raving mad!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> ( do 
            w1 <- pString "is stricken by melancholy!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
pLogEntryData OtherLogEntry         t@LEMoodInsane = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEMoodTantrum = do
    try ( do 
            w1 <- pString "is throwing a tantrum"
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>w2, ""]
            )
        <|> try ( do 
            w1 <- pString "has calmed down."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1, ""]
            )
        <|> ( do 
            w1 <- pString "cancels"
            space
            j <- pTillChars ":"
            w2 <- pString " Throwing tantrum."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & job ?~ j
                & strs .~ [w1, tcol<>w2]
            )
pLogEntryData OtherLogEntry         t@LEMoodTantrum = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEMoodDepression = do
    try ( do 
            w1 <- pString "is stumbling around obliviously!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> ( do 
            w1 <- pString "has slipped into depression..."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
pLogEntryData OtherLogEntry         t@LEMoodDepression = fail ""
pLogEntryData _ t@LEGuild =
    try ( do
        w1 <- pString "The "
        w2 <- pSomething ["guild"]
        w3 <- pString "guild, has been established."
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2<>ts<>w3]
        )
    <|> try ( do
        w1 <- pString "The guildhall agreement with "
        w2 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2]
        )
    <|> ( do
        w1 <- pString "The priesthood of "
        w2 <- pSomething [ "is" ]
        w3 <- pString "is ready to be recognized "
        w4 <- pAny
        return $ newLogEntryData & tag .~ t
            & strs .~ [w1<>w2<>ts<>w3<>w4]
        )
pLogEntryData (ActorFirst w1 acA)   t@LEBattleBreath = do
    -- w1 <- pString "The"
    -- space
    try ( do
            -- a1 <- pSomeone [ "breathes", "hurls", "is", "blocks", "shoots" ]
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
                & ac1 ?~ acA
                & strs .~ [w1, w2<>w3]
            )
        <|> ( do
            -- a1 <- pSomeone [ "is" ]
            w2 <- pString "is caught in a "
            w3 <- try (pString "cloud of ") <|> pString "burst of "
            w4 <- pSomething [ "extract" ]
            w5 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1, w2<>w3<>w4<>ts<>w5]
            )
pLogEntryData OtherLogEntry         t@LEBattleBreath = fail ""
pLogEntryData _ t@LEMasterpieceLost = do
    w1 <- pString "A masterwork of "
    w2 <- pSomething ["has"]
    w3 <- pString "has been lost!"
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2<>ts<>w3]
pLogEntryData (ActorFirst _ acA)    t@LEHazard = do
    w2 <- pString "is caught in a "
    w3 <- pTillChars "!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w2<>w3]
pLogEntryData OtherLogEntry         t@LEHazard = fail ""
pLogEntryData _ t@LEMiningWarning = do
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
pLogEntryData _ t@LEMigrants = do
    w1 <- try ( pString "The fortress attracted no migrants")
        <|> try (pString "No one even considered making the")
        <|> try (pString "A migrant has arrived.")
        <|> try ( do 
                w1' <- pString "Some migrants have "
                w1'' <- try (pString "arrived")
                    <|> pString "decided to brave this"
                return $ w1'<>w1''
                )
    w2 <- pAny                
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1<>w2]
pLogEntryData _ t@LESettlement = do
    try ( do
            w1 <- pSomething [ "and" ]
            w2 <- pString "and the surrounding lands have been made a"
            w3 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2<>w3]
            )
        <|> try ( do
            w1 <- pString "Siege was broken."
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1]
            )
        <|> try ( do
            w1 <- pSomething [ "was" ]
            w2 <- pString "was built."
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2]
            )
        <|> ( do
            w1 <- pString "The hillocks of "
            w2 <- pSomething [ "has" ]
            w3 <- pString "has been founded "
            w4 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2<>ts<>w3<>w4]
            )
pLogEntryData (ActorFirst _ acA)    t@LEDeath = do
    w1 <- try ( do
            w2' <- pString "has been "
            w3' <- try (pString "struck down")
                <|> try (pString "crushed by a drawbridge")
                <|> try (pString "encased in ice")
                <|> try (pString "encased in cooling lava")
                <|> try (pString "shot and killed")
                <|> try (pString "impaled on spikes")
                <|> try (pString "killed by a flying object")
                <|> try (pString "killed by a trap")
                <|> try (pString "murdered by ")
                <|> pString "scared to death by the "
            w4' <- pAny
            return $ w2'<>w3'<>w4'
            )
        <|> try ( do
            w2' <- pString "has died "
            w3' <- try (pString "of thirst")
                <|> try (pString "from thirst")
                <|> try (pString "after colliding with an obstacle")
                <|> pString "of old age"
            w4' <- pAny
            return $ w2'<>w3'<>w4'
            )
        <|> try ( do
            w2' <- pString "has "
            w3' <- try (pString "succumbed to infection.")
                <|> try (pString "bled to death.")
                <|> try (pString "starved to death.")
                <|> try (pString "suffocated.")
                <|> try (pString "drowned.")
                <|> pString "collapsed."
            return $ w2'<>w3'
            )
        <|> pString "slams into an obstacle and blows apart!"
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1]
pLogEntryData OtherLogEntry         t@LEDeath = fail ""
pLogEntryData (ActorFirst _ acA)    t@LEDeathFound = do
    try ( do
            w1 <- pString "has been found dead"
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1<>w2]
            )
        <|> try ( do
            w1 <- pString "has been missing for a week."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
        <|> ( do
            w1 <- pString "has been found, starved to death."
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
pLogEntryData OtherLogEntry         t@LEDeathFound = fail ""
pLogEntryData (ActorFirst _ acA)    t@LENecromancy = do
    try ( do
            w1 <- pString "gestures!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
            )
    <|> ( do
            w1 <- pString "shudders and begins to move!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & strs .~ [w1]
        )
pLogEntryData OtherLogEntry         t@LENecromancy = do
    w1 <- pString "You gesture!"
    return $ newLogEntryData & tag .~ t
        & strs .~ [w1]
pLogEntryData _ t@LEIntruders = do
    try ( do 
            w1 <- try (pString "The dead walk.  Hide while you still can!")
                <|> try (pString "A vile force of darkness has arrived!")
                <|> try (pString "An ambush!  Curse them!")
                <|> try (pString "An ambush!  Drive them out!")
                <|> try (pString "A kidnapper has made off with")
                <|> try (pString "Thief!  Protect the hoard from skulking filth!")
            w2 <- pAny
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>w2]
            )
        <|> try ( do
            w1 <- pString "Snatcher!"
            space
            w2 <- option "" (try (pSomething [ "Protect" ]))
            w3 <- pString "Protect the children!"
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2<>ts<>w3]
            )
        <|> try ( do
            w1 <- try (pString "A") <|> pString "An"
            space
            w2 <- pTillChars "!"
            w3 <- pString "  Drive it away!"
            return $ newLogEntryData & tag .~ t
                & strs .~ [w1<>ts<>w2<>texcl<>w3]
            )
        <|> try ( do
            acA <- pActor [ "batters" ]
            w1 <- pString "batters"
            space
            m <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & mat ?~ m
                & strs .~ ["battered by"]
            )
        <|> try ( do    -- TODO: test this
            acA <- pActor [ "has" ]
            w1 <- pString "has stolen"
            space
            m <- pTillChars "!"
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & mat ?~ m
                & strs .~ ["has stolen by"]
            )
        <|> ( do
            m <- pSomething [ "destroyed", "toppled" ]  -- TODO: may be another tag?
            w1 <- try (pString "destroyed by") <|> pString "toppled by"
            space
            acA <- pActor []
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ acA
                & mat ?~ m
                & strs .~ [w1]
            )
pLogEntryData (ActorFirst _ acA)    t@LEGhost = do
    w1 <- try (pString "has risen and is haunting the fortress")
        <|> try (pString "can be heard howling throughout the fortress")
        <|> try (pString "has been put to rest")
        <|> try (pString "has grown to become a Ghost")
        <|> pString "is following"
    w2 <- pAny
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ ["", "", w1<>w2]
pLogEntryData OtherLogEntry         t@LEGhost = do
    w1 <- pString "a"
    space
    m <- pSomething [ "has" ]
    w2 <- pString "has been misplaced.  No doubt "
    acA <- pSomeoneNoTrim [ "," ]
    w3 <- pAny
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ Creature acA
        & mat ?~ m
        & strs .~ [w1, w2, w3]
pLogEntryData (ActorFirst _ acA)    t@LEWerebeast = do
    w1 <- pString "has transformed into a"
    w2 <- pAny
    return $ newLogEntryData & tag .~ t
        & ac1 ?~ acA
        & strs .~ [w1<>w2]
pLogEntryData OtherLogEntry         t@LEWerebeast = fail ""
pLogEntryData _ t@LETitan = do
    try ( do 
            w1 <- pString "The Forgotten Beast"
            acA <- pSomeone ["has"]
            w2 <- pString "has come!"
            w3 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature acA
                & strs .~ [w1, w2<>w3]
            )
        <|> try ( do
            w1 <- pString "The"
            space
            w2 <- pSomething ["Titan"]
            w3 <- pString "Titan"
            space
            acA <- pSomeone ["has"]
            w4 <- pString "has come!"
            w5 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature acA
                & strs .~ [w1<>ts<>w2<>ts<>w3, w4<>w5]
            )
        <|> ( do
            w1 <- pString "The"
            space
            acA <- pSomeone ["has"]
            w2 <- pString "has come!  "
            w3 <- try (pString "A") <|> pString "An"
            w4 <- pAny
            return $ newLogEntryData & tag .~ t
                & ac1 ?~ Creature acA
                & strs .~ [w1, w2<>w3<>w4]
            )
pLogEntryData _ t@LESeason = do
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
pLogEntryData _ t@LESystem =
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

getActorFirst :: Parsec Text LogParseConfig LogEntryStructure
getActorFirst = do
    !w1 <- option "" (try (pString "The"))
    spaces
    !acA <- pActor 
        [ "has", "cancels", "and", "is", "from", "have", "begins"
        , "withdraws", "can", "looses", "gestures", "shudders", "became"
        , "slams", "jumps", "scrambles", "rolls", "falls", "bats"
        ]
    return $ ActorFirst w1 acA

getCreatureFirst :: Parsec Text LogParseConfig LogEntryStructure
getCreatureFirst = do
    -- !w1 <- pString "The"
    -- space
    !w1 <- option "" (try (pString "The"))
    spaces
    !acA <- pSomeone
        [ "attacks", "strikes", "misses", "blocks", "charges", "collides"
        , "has", "is", "stands", "passes", "falls", "regains"
        , "bounces", "collapses", "looks", "cancels"
        , "leaps", "punches", "catches", "snatches", "stabs"
        , "grabs", "hacks", "pushes", "slashes", "shakes"
        , "gores", "strangles", "scratches", "kicks"
        , "lashes", "slaps", "bashes", "bites", "strikes"
        , "releases", "throws", "takes", "locks", "bends", "places", "gouges"
        , "skids", "gives", "passes", "vomits", "retches", "regains"
        , "trouble", "skips", "rolls", "unrolls"
        , "counterstrikes!", "slams", "pulls", "loses"
        , "latches", "lets", "manages", "adjusts", "breaks"
        , "struggles", "takes", "rushes"
        , "breathes", "hurls", "shoots"        
        ]
    return $ ActorFirst w1 (Creature acA)

-- | Base parsing rule; place move specific and more friquent rules to top
baseRule :: Parsec Text LogParseConfig LogEntryData
baseRule = 
    try ( do
        les <- getCreatureFirst
        try (pLogEntryData les LEBattleMiss)
            <|> try (pLogEntryData les LEBattleEvent)
            <|> try (pLogEntryData les LEBattleStrike)
            <|> try (pLogEntryData les LEBattleStatus)
            <|> try (pLogEntryData les LEBattleEvent2)
            <|> pLogEntryData les LEBattleBreath
        )
    <|> try ( do
        les <- getActorFirst
        try (pLogEntryData les LEJobSuspension)         -- needs to be on top of LECraftCancel and LEJobCancel
            <|> try (pLogEntryData les LECraftCancel)   -- needs to be beetwean of LEJobSuspension and LEJobCancel
            <|> try (pLogEntryData les LEJobCancel)     -- needs to be on bottom of of LEJobSuspension and LECraftCancel
            <|> try (pLogEntryData les LEMasterpieceImproved)
            <|> try (pLogEntryData les LEMasterpieceCreated)
            <|> try (pLogEntryData les LEBattleEvade)
            <|> try (pLogEntryData les LEBattleTrance)
            <|> try (pLogEntryData les LEEmotion)
            <|> try (pLogEntryData les LESocial)
            <|> try (pLogEntryData les LESomeoneBecome)
            <|> try (pLogEntryData les LEMandate)
            <|> try (pLogEntryData les LETrade)
            <|> try (pLogEntryData les LEVisit)
            <|> try (pLogEntryData les LESting)
            <|> try (pLogEntryData les LEItem)
            <|> try (pLogEntryData les LESkillLevel)
            <|> try (pLogEntryData les LEMoodNormal)
            <|> try (pLogEntryData les LEMoodInsane)
            <|> try (pLogEntryData les LEMoodTantrum)
            <|> try (pLogEntryData les LEMoodDepression)
            <|> try (pLogEntryData les LEHazard)        -- must be on bottom of LEBattleBreath
            <|> try (pLogEntryData les LEDeath)
            <|> try (pLogEntryData les LEDeathFound)
            <|> try (pLogEntryData les LENecromancy)
            <|> try (pLogEntryData les LEGhost)
            <|> pLogEntryData les LEWerebeast
        )
    <|> ( do
        let les = OtherLogEntry
        try (pLogEntryData les LEJobSuspension)         -- needs to be on top of LECraftCancel and LEJobCancel
            <|> try (pLogEntryData les LECraftCancel)   -- needs to be beetwean of LEJobSuspension and LEJobCancel
            <|> try (pLogEntryData les LEJobCancel)     -- needs to be on bottom of of LEJobSuspension and LECraftCancel
            <|> try (pLogEntryData les LEProductionCompleted)
            <|> try (pLogEntryData les LEMasterpieceImproved)
            <|> try (pLogEntryData les LEMasterpieceCreated)
            <|> try (pLogEntryData les LECrimeTheft)
            <|> try (pLogEntryData les LEDFHackAutomation)
            <|> try (pLogEntryData les LEMiningStruck)
            <|> try (pLogEntryData les LEBattleMiss)
            <|> try (pLogEntryData les LEBattleEvent)
            <|> try (pLogEntryData les LEBattleStrike)
            <|> try (pLogEntryData les LEBattleHit)
            <|> try (pLogEntryData les LEBattleEvade)
            <|> try (pLogEntryData les LEBattleStatus)
            <|> try (pLogEntryData les LEBattleEvent2)
            <|> try (pLogEntryData les LEBattleTrance)
            <|> try (pLogEntryData les LEEmotion)
            <|> try (pLogEntryData les LEGore)
            <|> try (pLogEntryData les LEAnimalGrown)
            <|> try (pLogEntryData les LEAnimalBirth)
            <|> try (pLogEntryData les LEAnimalSlaughtered)
            <|> try (pLogEntryData les LEAnimalTraining)
            <|> try (pLogEntryData les LEAnimalMisc)
            <|> try (pLogEntryData les LESocial)
            <|> try (pLogEntryData les LESomeoneBecome)
            <|> try (pLogEntryData les LEMandate)
            <|> try (pLogEntryData les LETrade)
            <|> try (pLogEntryData les LEVisit)
            <|> try (pLogEntryData les LESting)
            <|> try (pLogEntryData les LEItem)
            <|> try (pLogEntryData les LEWeather)
            <|> try (pLogEntryData les LEFishing)
            <|> try (pLogEntryData les LEAdoption)
            <|> try (pLogEntryData les LESkillLevel)
            <|> try (pLogEntryData les LEMoodNormal)
            <|> try (pLogEntryData les LEMoodInsane)
            <|> try (pLogEntryData les LEMoodTantrum)
            <|> try (pLogEntryData les LEMoodDepression)
            <|> try (pLogEntryData les LEGuild)
            <|> try (pLogEntryData les LEBattleBreath)  -- must be on top of LEHazard
            <|> try (pLogEntryData les LEMasterpieceLost)
            <|> try (pLogEntryData les LEHazard)        -- must be on bottom of LEBattleBreath
            <|> try (pLogEntryData les LEMiningWarning)
            <|> try (pLogEntryData les LEMigrants)
            <|> try (pLogEntryData les LESettlement)
            <|> try (pLogEntryData les LEDeath)
            <|> try (pLogEntryData les LEDeathFound)
            <|> try (pLogEntryData les LENecromancy)
            <|> try (pLogEntryData les LEIntruders)
            <|> try (pLogEntryData les LEGhost)
            <|> try (pLogEntryData les LEWerebeast)
            <|> try (pLogEntryData les LETitan)
            <|> try (pLogEntryData les LESeason)
            <|> try (pLogEntryData les LESystem)
            <|> pLogEntryData les LEDefault             -- must be the last of all
        )
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

