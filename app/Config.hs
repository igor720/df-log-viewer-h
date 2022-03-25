{-|
Module      : Config
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Application's main config module
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import Control.Exception
import qualified Control.Lens as CL
import Control.Lens.TH ( makeLenses )
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Text as T
import Data.Text ( Text )
import Data.YAML
import TextShow
import Numeric ( showHex )
import Monomer ( rgbHex, Color )
import qualified Monomer.Lens as L

import AppException
import LogParser.LogEntry


type Secs = Int

type Width = Double

fontsPath :: FilePath
fontsPath = "assets/fonts"

instance FromYAML ShowNameType where
    parseYAML = withStr "" $ \t -> do 
        let sn
                | t=="FullName"     = SNFullName
                | t=="NameOnly"     = SNNameOnly
                | t=="NicknameOnly" = SNNicknameOnly
                | otherwise = throw $ ExInvalidConfigRecord
                    "invalid 'showName' configuration value"
        return sn

instance ToYAML ShowNameType where
    toYAML sn = let _:_:str = show sn in toYAML (T.pack str)

-- | Application's main configuration
data MainConfig = MainConfig
    { _acMainWindowDefaultSize  :: (Int, Int)
    , _acRegularFont            :: Text
    , _acEmphasizeFont          :: Text
    , _acTextSize               :: Double
    , _acTimeColors             :: (Color, Color, Color)
    , _acTimeColorShifts        :: (Int, Int)
    , _acExplicitSecs           :: Secs
    , _acSpacerWidth            :: Width
    , _acTimeFieldWidth         :: Width
    , _acLogWindows             :: Int
    , _acColorSampleText        :: Text
    , _acBgColor                :: Maybe Color
    , _acJobDecorUnderline      :: Bool
    , _acJobDecorFgColor        :: Maybe Color
    , _acJobDecorBgColor        :: Maybe Color
    , _acMatDecorUnderline      :: Bool
    , _acMatDecorFgColor        :: Maybe Color
    , _acMatDecorBgColor        :: Maybe Color
    , _acDorfDecorUnderline     :: Bool
    , _acDorfDecorFgColor       :: Maybe Color
    , _acDorfDecorBgColor       :: Maybe Color
    , _acPreviousLogEntries     :: Int
    , _acLogFilePath            :: Maybe Text
    , _acShowProfession         :: Bool
    , _acShowName               :: ShowNameType
    , _acColoredTag             :: Bool
    , _acFadeAnimationDuration  :: Int
    } deriving (Show, Eq)

makeLenses 'MainConfig

instance FromYAML Color where
    parseYAML = withStr "Color" $ \t -> pure (rgbHex (T.unpack t))

instance ToYAML Color where
    toYAML color = toYAML $ "#"
        <> T.pack (showHex (color CL.^. L.r) "")
        <> T.pack (showHex (color CL.^. L.g) "")
        <> T.pack (showHex (color CL.^. L.b) "")

instance FromYAML MainConfig where
    parseYAML = withMap "MainConfig" $ \m -> MainConfig
        <$> m .: "mainWindowDefaultSize"
        <*> m .: "regularFont"
        <*> m .: "emphasizeFont"
        <*> m .: "textSize"
        <*> m .: "timeColors"
        <*> m .: "timeColorShifts"
        <*> m .: "explicitSecs"
        <*> m .: "spacerWidth"
        <*> m .: "timeFieldWidth"
        <*> m .: "logWindows"
        <*> m .: "colorSampleText"
        <*> m .:? "bgColor" .!= Nothing
        <*> m .: "jobDecorUnderline"
        <*> m .:? "jobDecorFgColor" .!= Nothing
        <*> m .:? "jobDecorBgColor" .!= Nothing
        <*> m .: "matDecorUnderline"
        <*> m .:? "matDecorFgColor" .!= Nothing
        <*> m .:? "matDecorBgColor" .!= Nothing
        <*> m .: "dorfDecorUnderline"
        <*> m .:? "dorfDecorFgColor" .!= Nothing
        <*> m .:? "dorfDecorBgColor" .!= Nothing
        <*> m .:? "previousLogEntries" .!= 0
        <*> m .:? "logFilePath" .!= Nothing
        <*> m .: "showProfession"
        <*> m .: "showName"
        <*> m .: "coloredTag"
        <*> m .: "fadeAnimationDuration"

instance ToYAML MainConfig where
    toYAML (MainConfig mws rf ef ts tc tcs es sw tfw ws cst bc
                jdu jdfg jdbg mdu mdfg mdbg ddu ddfg ddbg 
                ple lfp snP sn ct fad) = mapping 
        [ "mainWindowDefaultSize"   .= mws
        , "regularFont"             .= rf
        , "emphasizeFont"           .= ef
        , "textSize"                .= ts
        , "timeColors"              .= tc
        , "timeColorShifts"         .= tcs
        , "explicitSecs"            .= es
        , "spacerWidth"             .= sw
        , "timeFieldWidth"          .= tfw
        , "logWindows"              .= ws
        , "colorSampleText"         .= cst
        , "bgColor"                 .= bc
        , "jobDecorUnderline"       .= jdu
        , "jobDecorFgColor"         .= jdfg
        , "jobDecorBgColor"         .= jdbg
        , "matDecorUnderline"       .= mdu
        , "matDecorFgColor"         .= mdfg
        , "matDecorBgColor"         .= mdbg
        , "dorfbDecorUnderline"     .= ddu
        , "dorfDecorFgColor"        .= ddfg
        , "dorfDecorBgColor"        .= ddbg
        , "previousLogEntries"      .= ple
        , "logFilePath"             .= lfp
        , "showProfession"          .= snP
        , "showName"                .= sn
        , "coloredTag"              .= ct
        , "fadeAnimationDuration"   .= fad
        ]

writeMainConfig :: FilePath -> MainConfig -> IO ()
writeMainConfig path cfg = do
    BS.L.writeFile path (encode1 cfg)
        `catch` \(e::SomeException) -> throw ExSaveMainConfig

readMainConfig :: FilePath -> IO MainConfig
readMainConfig path = do
    raw <- catch (
        BS.L.readFile path
        ) (\(e::SomeException) -> throw (ExReadMainConfig $ show e))
    case decode1 raw of
        Left (loc, emsg)    -> throw $ 
            ExDecodeMainConfig (prettyPosWithSource loc raw "") emsg
        Right cfg           -> pure cfg

checkMainConfig :: MainConfig -> Either String MainConfig
checkMainConfig cfg@(MainConfig (w,h) _ _ ts _ (tsh0,tsh1) es sw tfw ws cst _
                        _ _ _ _ _ _ _ _ _ 
                        ple _ snP sn _ fad)
    | w<400 || h<300    = Left "Too small default window size"
    | ts<7 || ts>36     = Left "Too big font"
    | any (<0) [tsh0, tsh1] = 
                          Left "Invalid time color shifts specification"
    | es<0              = Left "'explicitSecs' must be non-negative integer"
    | sw<0              = Left "'spacerWidth' must be non-negative integer"
    | tfw<10 || tfw>40  = Left "'timeFieldWidth' must be in integval [10, 40]"
    | ws<1 || ws>4      = Left "'windows' must be in integval [1, 4]"
    | T.length cst<1 || T.length cst>16 = 
                          Left $ "Length of 'colorSampleText' must be "
                              <> "in interval [1, 16]"
    | ple<0             = Left "previousLogEntries must be non-negative integer"
    | fad<0             = Left "'fadeAnimationDuration' must be non-negative"
    | otherwise         = Right cfg


