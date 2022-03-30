{-|
Module      : LogParser.Profession
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Professions handle module. Currently is not in use
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module LogParser.Profession where

import Control.Exception
import Data.Text ( Text, pack, unpack )
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
--import qualified Text.Parsec as Parsec

import LogException


data ProfessionId = 
        PrAdministrator | PrAlchemist | PrAnimalCaretaker | PrAnimalDissector | PrAnimalTrainer
        | PrArchitect | PrArmorer | PrAstronomer | PrAxeLord | PrAxeman
        | PrBaby | PrBard | PrBeastHunter | PrBeekeeper | PrBlacksmith | PrBlowgunner | PrBoneCarver
        | PrBoneDoctor | PrBookbinder | PrBowman | PrBowyer | PrBrewer | Prbroker | PrButcher
        | PrCarpenter | PrCheeseMaker | PrChemist | PrChild | PrClerk | PrClothier
        | PrCook | PrCraftsdwarf | PrCraftsman | PrCriminal | PrCrossbowman
        | PrDancer | PrDiagnoser | PrDoctor | PrDrunk | PrDwarvenChild | PrDyer
        | PrEliteBowman | PrEliteCrossbowman | PrEliteWrestler | PrEngineer | PrEngraver
        | PrFarmer | PrFishCleaner | PrFishDissector | PrFisherman | PrFisheryWorker | PrFurnaceOperator
        | PrGelder | PrGemCutter | PrGemSetter | PrGeographer | PrGlassmaker | PrGlazer
        | PrHammerLord | PrHammerman | PrHerbalist | PrHistorian | PrHunter | PrHuntingAnimal
        | PrJeweler
        | PrLasher | PrLeatherworker | PrLyeMaker
        | PrMaceLord | PrMaceman | PrMason | PrMasterBlowgunner | PrMasterLasher | PrMasterThief
        | PrMathematician | PrMechanic | PrMercenary | PrMerchant | PrMessenger | PrMetalcrafter
        | PrMetalsmith | PrMilker | PrMiller | PrMiner | PrMonk | PrMonsterSlayer
        | PrNaturalist
        | PrPapermaker | PrPeasant | PrPeddler | PrPerformer | PrPhilosopher | PrPikeman | PrPikemaster
        | PrPilgrim | PrPlanter | PrPoet | PrPotashMaker | PrPotter | PrPresser | PrProphet | PrPumpOperator
        | PrRanger | PrRecruit
        | PrSage | PrScholar | PrScout | PrScribe | PrShearer | PrSiegeEngineer | PrSiegeOperator
        | PrSnatcher | PrSoapMaker | PrSpearman | PrSpearmaster | PrSpinner | PrStonecrafter
        | PrStoneworker | PrStrandExtractor | PrSurgeon | PrSuturer | PrSwordmaster | PrSwordsman
        | PrTanner | PrTavernKeeper | PrThief | PrThresher | PrTrader | PrTrapper
        | PrWarAnimal | PrWaxWorker | PrWeaponsmith | PrWeaver | PrWoodBurner | PrWoodcrafter
        | PrWoodcutter | PrWoodworker | PrWrestler
    deriving (Show, Eq, Ord, Enum, Bounded)

-- $(deriveTextShow ''Profession)

type ProfessionName = Text

type ProfIdNameMapping = M.Map ProfessionId ProfessionName
type ProfNameIdMapping = M.Map ProfessionName ProfessionId

makeProfIdNameMapping :: ProfIdNameMapping
makeProfIdNameMapping = mapping where
    ps = enumFrom minBound :: [ProfessionId]
    mapping = M.fromList $ 
        List.map (\p -> (p, let _:_:str = show p in pack str)) ps

makeProfNameIdMapping :: ProfNameIdMapping
makeProfNameIdMapping = mapping where
    ps = enumFrom minBound :: [ProfessionId]
    mapping = M.fromList $ 
        List.map (\p -> (let _:_:str = show p in pack str, p)) ps

getProfessionName :: ProfessionId -> ProfessionName
getProfessionName pId = fromMaybe 
    (throw $ ExMissedProfessionId pId)
    (M.lookup pId makeProfIdNameMapping)

getProfessionId :: ProfessionName -> ProfessionId
getProfessionId pName = fromMaybe 
    (throw $ ExMissedProfessionName pName)
    (M.lookup pName makeProfNameIdMapping)

newtype ExMissedProfessionId = ExMissedProfessionId ProfessionId

instance Show ExMissedProfessionId where
    show (ExMissedProfessionId pId) = "missed profession id: "++show pId

instance Exception ExMissedProfessionId where
    toException   = logExceptionToException
    fromException = logExceptionFromException

newtype ExMissedProfessionName = ExMissedProfessionName ProfessionName

instance Show ExMissedProfessionName where
    show (ExMissedProfessionName pName) = "missed profession: "++unpack pName

instance Exception ExMissedProfessionName where
    toException   = logExceptionToException
    fromException = logExceptionFromException

