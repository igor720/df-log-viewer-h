{-# LANGUAGE OverloadedStrings #-}

module Test.Rules where

import Test.HUnit
import System.FilePath
import Data.Text (Text)
import TextShow (TextShow(showt))

import LogParser.Rules.Helpers
import LogParser.Rules
import LogParser.LogEntry


tpLogEntryData :: Test
tpLogEntryData = TestList
    [ let tag=LEDefault in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "default rule parsing" ~?= 
            LogEntryData tag 
                Nothing 
                Nothing 
                Nothing 
                Nothing 
                ["default rule parsing"]
    , let tag=LEJobSuspension in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Doctor' Ducimmuzish, Suturer cancels Construct Building: Building site submerged." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Ducimmuzish", _nickname = Just "Doctor", _prof = "Suturer"}))
                Nothing
                (Just "Construct Building")
                Nothing
                ["","Building site submerged",":"]
    , let tag=LEJobSuspension in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The dwarves suspended a linkage from Pressure Plate." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                (Just "linkage from")
                (Just "Pressure Plate")
                ["","",""]
    , let tag=LEJobSuspension in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The dwarves suspended the construction of Wall." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                (Just "construction")
                (Just "Wall")
                ["","",""]
    , let tag=LECraftCancel in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Gem Setter' Idstorlut, Gem Setter cancels Encrust Finished Goods With resin opal: Needs resin opals." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Idstorlut", _nickname = Just "Gem Setter", _prof = "Gem Setter"}))
                Nothing 
                (Just "Encrust Finished Goods With resin opal")
                (Just "resin opals")
                []
    , let tag=LECraftCancel in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Carpenter' Shorastsanreb, broker cancels Construct coconut palm Bin: Needs coconut palm logs." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Shorastsanreb", _nickname = Just "Carpenter", _prof = "broker"}))
                Nothing 
                (Just "Construct coconut palm Bin")
                (Just "coconut palm logs")
                []
    , let tag=LEJobCancel in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Cook' Odomkivish, Farmer cancels Prepare Lavish Meal: Needs unrotten cookable solid item." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Odomkivish", _nickname = Just "Cook", _prof = "Farmer"}))
                Nothing
                (Just "Prepare Lavish Meal")
                Nothing
                ["Needs unrotten cookable solid item"]
    , let tag=LEProductionCompleted in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Shear Animal (1) has been completed." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                (Just "Shear Animal")
                (Just "1")
                []
    , let tag=LEMasterpieceImproved in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Stѓkud Idstorlut has improved a 1mussel shell earring1 masterfully!" ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Stѓkud Idstorlut", _nickname = Nothing, _prof = ""}))
                Nothing 
                Nothing 
                (Just "1mussel shell earring1")
                []
    , let tag=LEMasterpieceCreated in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Mason' RithlutatЎr has constructed a masterpiece!" ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "RithlutatЎr", _nickname = Just "Mason", _prof = ""}))
                Nothing 
                Nothing 
                Nothing 
                ["has constructed a masterpiece!"]
    , let tag=LEDeathFound in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Hunter' Zimed‰m, Hunter has been found dead, drowned." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Zimed‰m", _nickname = Just "Hunter", _prof = "Hunter"}))
                Nothing
                Nothing
                Nothing
                ["drowned"]
    , let tag=LECrimeTheft in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Zaledenseb Ilb�d Bukith is missing from its proper place!" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                (Just "Zaledenseb Ilb�d Bukith")
                ["is missing from its proper place!"]
    , let tag=LEDFHackAutomation in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Marked 47 items to melt" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                (Just "melt")
                (Just "47")
                ["to"]
    , let tag=LEMiningStruck in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "You have struck citrine!" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                (Just "citrine")
                ["You have struck"]
    , let tag=LEBattleMiss in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The hammerdwarf strikes at the hammerdwarf but the shot is parried by the (bismuth bronze battle axe)!" ~?= 
            LogEntryData tag 
                (Just (Creature "hammerdwarf"))
                (Just (Creature "hammerdwarf"))
                Nothing
                Nothing
                ["The","strikes at the","but the shot is parried by the (bismuth bronze battle axe)!"]
    , let tag=LEBattleMiss in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The axedwarf misses the hammerdwarf!" ~?= 
            LogEntryData tag 
                (Just (Creature "axedwarf"))
                (Just (Creature "hammerdwarf"))
                Nothing
                Nothing
                ["The","misses the","!"]
    , let tag=LEBattleEvent in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The stray war dog charges at the weasel!" ~?= 
            LogEntryData tag 
                (Just (Creature "stray war dog"))
                (Just (Creature "weasel"))
                Nothing
                Nothing
                ["The","charges at","!"]
    , let tag=LEBattleEvent in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The hammerdwarf is knocked over!" ~?= 
            LogEntryData tag 
                (Just (Creature "hammerdwarf"))
                (Just Nobody)
                Nothing
                Nothing
                ["The","is knocked over!",""]
    , let tag=LEBattleEvent in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The stray kitten is knocked over and tumbles backward!" ~?= 
            LogEntryData tag 
                (Just (Creature "stray kitten"))
                (Just Nobody)
                Nothing
                Nothing
                ["The","is knocked over and tumbles backward!",""]
                
    , let tag=LEBattleStrike in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The macedwarf bashes the axedwarf in the left upper arm with her (+®copper maceЇ+), lightly tapping the target!" ~?= 
            LogEntryData tag 
                (Just (Creature "macedwarf"))
                (Just (Creature "axedwarf"))
                Nothing
                Nothing
                ["bashes","in the left upper arm with her (+\174copper mace\1031+), lightly tapping the target!"]
    , let tag=LEBattleHit in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The force twists the left hip, tearing apart the muscle and bruising the bone and tearing apart the muscle and bruising the bone!" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["The force twists the left hip, tearing apart the muscle and bruising the bone and tearing apart the muscle and bruising the bone!"]
    , let tag=LEBattleEvade in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The osprey jumps away from The flying ({bismuth bronze bolt})!" ~?= 
            LogEntryData tag 
                (Just (Creature "osprey"))
                Nothing
                Nothing
                Nothing
                ["The","jumps away from The flying ({bismuth bronze bolt})!",""]
    , let tag=LEBattleStatus in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The dingo vomits." ~?= 
            LogEntryData tag 
                (Just (Creature "dingo"))
                Nothing
                Nothing
                Nothing
                ["vomits."]
    , let tag=LEGore in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "An artery has been opened by the attack and a tendon has been torn!" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["An artery has been opened by the attack and a tendon has been torn!"]
    , let tag=LEGore in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "A ligament in the left wrist has been torn and a tendon has been torn!" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["A ligament in the left wrist has been torn and a tendon has been torn!"]
    , let tag=LEAnimalGrown in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "An animal has grown to become a Stray Cat." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                (Just "Stray Cat.")
                []
    , let tag=LEAnimalBirth in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Stray Horse (Tame) has given birth to a horse foal." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                (Just "Stray Horse (Tame)")
                ["has given birth to a horse foal."]
    , let tag=LEAnimalSlaughtered in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The Stray Turkey Hen (Tame) has been slaughtered." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                (Just "Stray Turkey Hen (Tame)")
                []
    , let tag=LESomeoneBecome in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Leather' UzoldЉg has become a Leatherworker." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Uzold\1033g", _nickname = Just "Leather", _prof = ""}))
                Nothing
                Nothing
                Nothing
                ["has become a Leatherworker"]
    , let tag=LESomeoneBecome in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Cheese Necro' Libashnanir, mayor necromancer has been re-elected." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Libashnanir", _nickname = Just "Cheese Necro", _prof = "mayor necromancer"}))
                Nothing
                Nothing
                Nothing
                ["has been re-elected."]
    , let tag=LESomeoneBecome in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "An animal has grown to become a Stray Water Buffalo Cow." ~?= 
            LogEntryData tag 
                (Just (Creature "animal"))
                Nothing
                Nothing
                (Just "Stray Water Buffalo Cow")
                ["has grown to become a"]
    , let tag=LESomeoneBecome in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "An animal has become a Stray war Dog." ~?= 
            LogEntryData tag 
                (Just (Creature "animal"))
                Nothing
                Nothing
                (Just "Stray war Dog")
                ["has become a"]
    , let tag=LEMandate in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Cheese Necro' Libashnanir, mayor necromancer has mandated the construction of certain goods." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Libashnanir", _nickname = Just "Cheese Necro", _prof = "mayor necromancer"}))
                Nothing
                Nothing
                Nothing
                ["has mandated the construction of certain goods."]
    , let tag=LEMandate in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Doctor' Tannish, duke of Springwhip has mandated the construction of certain goods." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Tannish", _nickname = Just "Doctor", _prof = "duke of Springwhip"}))
                Nothing
                Nothing
                Nothing
                ["has mandated the construction of certain goods."]
    , let tag=LETrade in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "A human caravan from Behal Agal has arrived." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["A human caravan from Behal Agal has arrived."]
    , let tag=LEVisit in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Imepe Lecececa, Elf Animal Caretaker fallen butcher is visiting." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Imepe Lecececa", _nickname = Nothing, _prof = "Elf Animal Caretaker"}))
                Nothing
                Nothing
                Nothing
                ["","fallen butcher is visiting."]
    , let tag=LEVisit in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Ormol Budojeha, Human Beast Hunter is visiting." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Ormol Budojeha", _nickname = Nothing, _prof = "Human Beast Hunter"}))
                Nothing
                Nothing
                Nothing
                ["","is visiting."]
    , let tag=LEVisit in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "A goblin diplomat from Behal Agal has arrived." ~?= 
            LogEntryData tag 
                (Just (Creature "goblin"))
                Nothing
                Nothing
                Nothing
                ["A","diplomat from Behal Agal has arrived."]
    , let tag=LESting in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "The stray war dog has been stung by a honey bee!" ~?= 
            LogEntryData tag 
                (Just (Creature "stray war dog"))
                Nothing
                Nothing
                Nothing
                ["has been stung by a honey bee!"]
    , let tag=LEItem in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Sniper' ‹ngizshoveth, Marksdwarf has grown attached to a iron crossbow!" ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "‹ngizshoveth", _nickname = Just "Sniper", _prof = "Marksdwarf"}))
                Nothing
                Nothing
                Nothing
                ["has grown attached to a iron crossbow!"]
    , let tag=LEWeather in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "A cloud of fire clay has drifted nearby!" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["A cloud of fire clay has drifted nearby!"]
    , let tag=LEFishing in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "There is nothing to catch in the southwestern swamps." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["There is nothing to catch in the southwestern swamps."]
    , let tag=LESeason in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Autumn has arrived on the calendar." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["Autumn has arrived on the calendar."]
    , let tag=LEAdoption in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Reg Mist€mtathtat, Stray Kitten (Tame) has adopted `Carpenter' Ronstizustuth, Carpenter." ~?= 
            LogEntryData tag 
                (Just (Creature "Reg Mist€mtathtat, Stray Kitten (Tame)"))
                (Just (Dorf {_name = "Ronstizustuth", _nickname = Just "Carpenter", _prof = "Carpenter"}))
                Nothing
                Nothing
                ["has adopted"]
    , let tag=LESkillLevel in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`New Warrior' Dodўkthabum, Axedwarf: I have improved my discipline.  That was satisfying." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Dodўkthabum", _nickname = Just "New Warrior", _prof = "Axedwarf"}))
                Nothing
                Nothing
                Nothing
                [": I have improved my discipline. That was very satisfying!"] 
    , let tag=LEMoodNormal in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Mason' Gireth—shrir, Mason withdraws from society..." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Gireth—shrir", _nickname = Just "Mason", _prof = "Mason"}))
                Nothing
                Nothing
                Nothing
                ["withdraws from society...",""] 
    , let tag=LEMoodInsane in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`Miner' Ingishshetb€th, Miner has been possessed!" ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Ingishshetb€th", _nickname = Just "Miner", _prof = "Miner"}))
                Nothing
                Nothing
                Nothing
                ["has been possessed!"] 
    , let tag=LESystem in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Loaded region1, Xemomon (The Mythical Realms) at coordinates (113,93)" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["Loaded region1, Xemomon (The Mythical Realms) at coordinates (113,93)"]
    , let tag=LESystem in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "*** STARTING NEW GAME ***" ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["*** STARTING NEW GAME ***"]
    , let tag=LEMiningWarning in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "Digging designation cancelled: damp stone located." ~?= 
            LogEntryData tag 
                Nothing
                Nothing
                Nothing
                Nothing
                ["Digging designation cancelled: damp stone located."]

    ] where
        cfg = LogParseConfig

--


tpLogEntryData1 :: Test
tpLogEntryData1 = TestList
    [ let tag=LESkillLevel in show tag ~: parseLogEntrySingle cfg (pLogEntryData tag) 
            "`New Warrior' Dodўkthabum, Axedwarf: I have improved my discipline.  That was satisfying." ~?= 
            LogEntryData tag 
                (Just (Dorf {_name = "Dodўkthabum", _nickname = Just "New Warrior", _prof = "Axedwarf"}))
                Nothing
                Nothing
                Nothing
                ["I have improved my discipline", "That was satisfying."]
    ] where
        cfg = LogParseConfig
