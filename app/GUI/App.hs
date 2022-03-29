{-|
Module      : GUI.App
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

App creation and functionality
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use maybe" #-}

module GUI.App where

import System.FilePath ( (</>) )
import System.IO
import Control.Exception
import Control.Lens
import Control.Monad.Loops ( iterateM_, unfoldM )
import Control.Concurrent ( threadDelay )
import Data.Default ( Default(def) )
import Data.Maybe ( fromMaybe, isJust, listToMaybe )
import Data.Text ( Text )
import TextShow ( TextShow(showt) )
import Data.Time ( UTCTime, diffUTCTime, getCurrentTime )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as List
import Monomer
import qualified Monomer.Lens as L

import Control.DeepSeq

import LogParser.LogEntry
import LogParser.Rules.Helpers
import LogParser.Rules
import LogParser.Reassemble
import Symbols
import AppException
import Config
import LogFile
import GUI.Model.ColorsDialog
import GUI.Model.LogWindowsDialog
import GUI.Model.App
import GUI.ColorsDialog
import GUI.LogWindowsDialog

{-# INLINE reforModeTxt #-}
{-# INLINE logWindowAdjustmentW #-}
{-# INLINE spaceComp #-}

reforModeTxt :: Text
reforModeTxt = "Log formatting.. Please wait."

spaceComp :: LEComponent
spaceComp = LEC LECOther " "

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

-- | Adjastment for log window width
logWindowAdjustmentW :: Int -> Double
logWindowAdjustmentW 1 = 28
logWindowAdjustmentW 2 = 28
logWindowAdjustmentW cols = throw $ ExLogColumns cols

-- | Time field color
defTimeColor :: (Int, Int) -> (Color, Color, Color) -> Secs -> Color
defTimeColor (tCSh0, tCSh1) (tC0, tC1, tC2) secs
    | secs <= tCSh0   = tC0
    | secs <= tCSh1   = tC1
    | otherwise       = tC2

-- | Returns text of time field, and its color
getLogEntryTimeValues :: AppModel -> LogEntry -> (Text, Color)
getLogEntryTimeValues model le = vals where
    time0 = model^. curTime
    colors@(_, _, timeColor) = model^. mainConfig.acTimeColors
    tcs = model^.mainConfig.acTimeColorShifts
    vals = case le^.leTime of
        Nothing     -> (">>", timeColor)
        Just time   -> res' where
            secs = secondsSince time0 time
            res' 
                | secs<=1 =
                    ( showt secs
                    , defTimeColor tcs colors secs
                    )
                | secs<=model^. mainConfig.acExplicitSecs =
                    ( showt secs
                    , defTimeColor tcs colors secs
                    )
                | otherwise = 
                    ( ">"
                    , defTimeColor tcs colors secs
                    )

-- Returns log entry text components, distributed among each line of text
getLogEntryTextComponents :: (Text -> Width) -> Width -> Width 
    -> ReLogEntry -> [[LEComponent]]
getLogEntryTextComponents getTextWidth introW logWindowW (ReLogEntry _ _ reas)
        = txtComps where
    spaceW = getTextWidth " "
    txtComps' = List.foldl' (\cws comp -> case cws of
        [] -> [([comp], introW+w)] where
            LEC _ txt = comp
            w = getTextWidth txt
        ((hComps0, w0) : cws') -> case comp of
            LEC _ txt  -> hComps where
                w = getTextWidth txt
                l = T.length txt
                hComps
                    | l==0    = (hComps0, w0) : cws' -- throw $ ExEmptyComponent reas
                    | l==1    = (comp:hComps0, w+w0) : cws' -- punctuation mark
                    | spaceW+w+w0<=logWindowW =
                                (comp:spaceComp:hComps0, w+spaceW+w0) : cws'
                    | otherwise = 
                                ([comp], introW+w) : (hComps0, w0) : cws'
        ) [] reas
    txtComps = reverse $ map (reverse . fst) txtComps'

logEntryRow :: AppWenv -> AppModel -> LogEntry -> AppNode
logEntryRow wenv model le = row where
    -- configuration parameters
    cfg = model^.mainConfig
    ws = model^.mainConfig.acLogWindows
    cols = if ws<=2 then 1 else 2 :: Int
    rowHoverBgColor = wenv^. L.theme. L.userColorMap. at "hoverBgColor". non def
    themeLabelStyle = wenv^. L.theme. L.basic. L.labelStyle
    spacerW = model^.mainConfig.acSpacerWidth
    timeW = model^.mainConfig.acTimeFieldWidth
    logWindowW = ((wenv^. L.windowSize. L.w)/fromIntegral cols) 
                    - logWindowAdjustmentW cols
    getTextWidth str = getTextSize wenv themeLabelStyle str^. L.w

    -- getting log entry components
    (timeTxt, timeColor) = getLogEntryTimeValues model le
    rle@(ReLogEntry leTag desc _) = le^. leReLogEntry
    descW = getTextSize wenv 
        (textFont "Bold"<>textSize (model^. mainConfig.acTextSize)) desc^. L.w
    introW = descW + timeW + spacerW
    txtComps = getLogEntryTextComponents getTextWidth introW logWindowW rle

    -- defining special text styles and constructing log entry text block
    txtColor = fromMaybe
        defaultColor
        (M.lookup leTag (model^. logColorDistrib))
    txtBlock = vstack ( map (
        hstack . map ( \(LEC lecId txt) -> case lecId of
                LECJob -> label txt `styleBasic` jobStyle
                LECMat -> label txt `styleBasic` matStyle
                LECDorf -> label txt `styleBasic` dorfStyle
                LECOther -> label txt `styleBasic` otherStyle
            )
        ) txtComps ) where
        jobStyle = [ textUnderline | cfg^. acJobDecorUnderline ]
            ++ [ textColor (fromMaybe txtColor (cfg^. acJobDecorFgColor))
               , bgColor (fromMaybe logWindowBgColor (cfg^. acJobDecorBgColor))
               ]
        matStyle = [ textUnderline | cfg^. acMatDecorUnderline ]
            ++ [ textColor (fromMaybe txtColor (cfg^. acMatDecorFgColor))
               , bgColor (fromMaybe logWindowBgColor (cfg^. acMatDecorBgColor))
               ]
        dorfStyle = [ textUnderline | cfg^. acDorfDecorUnderline ]
            ++ [ textColor (fromMaybe txtColor (cfg^. acDorfDecorFgColor))
               , bgColor (fromMaybe logWindowBgColor (cfg^. acDorfDecorBgColor))
               ]
        otherStyle = [textColor txtColor]

    -- constructing log entry visual row
    rowContent = box_ [alignTop, alignLeft] $ animFadeIn_ animCfg
        (hstack [
            box_ [alignRight, alignTop] (
                label_ timeTxt [ellipsis] `styleBasic` timeStyle
                ) `styleBasic` timeBoxStyle,
            box_ [] $ hstack [
                spacer_ [width spacerW],
                box_ [alignTop] (label desc `styleBasic` descStyle),
                spacer_ [width spacerW],
                txtBlock
                ]
            ]) `nodeKey` showt (le^. leId) where
        timeStyle = [ textColor timeColor ]
        timeBoxStyle = [ width timeW ]
        descStyle = [ textFont "Bold"
                    , textColor (if cfg^. acColoredTag then txtColor else white) 
                    ]
        animCfg = [duration (cfg^. acFadeAnimationDuration)]
    row = box_ [expandContent] (
            rowContent `styleBasic` basicStyle `styleHover` hoverStyle
            ) `styleBasic` boxStyle where
        basicStyle = []
        hoverStyle = [ bgColor rowHoverBgColor, cursorIcon CursorHand ]
        boxStyle = [ paddingH 5, paddingV 2 ]

logScreen :: AppWenv -> AppModel -> AppNode
logScreen wenv model = widgetTree where
    ws = model^.mainConfig.acLogWindows
    getWindow le = fromMaybe
        defaultLogWindow
        (M.lookup (le^. leReLogEntry.rleTag) (model^.logWindowDistrib))
    -- TODO: consider merging only changed log windows
    isMergeReqired _ newModel = newModel^.logMergeMode/=LMNo
    logWindow key w = vscroll_ [scrollFollowFocus, barWidth 10] 
        (vstack (logEntryRow wenv model <$> reverse ( 
            model^.logEntries.to getLogEntries ^..folded.filtered 
                (\le->(getWindow le==w) || (w==ws && getWindow le>ws))
        ))) `nodeKey` key
        `styleBasic` [ bgColor black, border 1 lightGray
                     , minHeight 100, minWidth 200, paddingB 12 ]
    logWindowsStructure
        | ws==1 = logWindow "Log1" 1
        | ws==2 = vsplit_ vsCfg
                ( logWindow "Log1" 1
                , logWindow "Log2" 2
                )
        | ws==3 = hstack
                [ vsplit_ vsCfg
                    (logWindow "Log1" 1
                    , logWindow "Log2" 2
                    )
                , spacer_ [width 4]
                , logWindow "Log3" 3
                ]
        | ws==4 = hstack
                [ vsplit_ vsCfg
                    ( logWindow "Log1" 1
                    , logWindow "Log2" 2
                    )
                , spacer_ [width 4]
                , vsplit_ vsCfg 
                    ( logWindow "Log3" 3
                    , logWindow "Log4" 4
                    )
                ]
        | otherwise = throw $ ExLogWindowsNumber ws
        where 
            vsCfg = [splitHandleSize 4, splitIgnoreChildResize True]
    widgetTree = vstack [
        box_ [mergeRequired isMergeReqired] $
            logWindowsStructure `styleBasic` []
        ] `styleBasic` [paddingH 2, paddingB 2]

buildUI :: WidgetEnv AppModel AppEvent -> AppModel 
    -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
    mainBgColor = wenv^. L.theme. L.userColorMap.at "bgColor".non def
    errorOverlay = alertMsg msg AppErrorClose where
        msg = fromMaybe "" (model ^. errorMsg)
    reformattingOverlay = 
        hstack [
            filler,
            label_ reforModeTxt [ellipsis] 
                `styleBasic` [ textSize 18, paddingH 40, paddingV 20, radius 10
                             , bgColor mainBgColor, fgColor (rgbHex "#222222")],
            filler
            ]
    logScreenLayer = vstack [
        hstack [
            spacer,
            hstack [mainButton "Colors" AppShowColorConfig] 
                `styleBasic` [ paddingH 5, paddingV 8 ],
            hstack [mainButton "Windows" AppShowWindowConfig] 
                `styleBasic` [ paddingH 5, paddingV 8 ]
            ] `styleBasic` [],
        logScreen wenv model
        ]
    colorsLayer = vstack [
        hstack [
            filler,
            hstack [mainButton "Apply" (AppCloseColorConfigScreen True)] 
                `styleBasic` [ paddingH 5, paddingV 8 ],
            hstack [button "Discard" (AppCloseColorConfigScreen False)]
                `styleBasic` [ paddingH 5, paddingV 8 ],
            spacer
            ] `styleBasic` [],
        colorsDialogWidget cModel (model^.mainConfig.acColorSampleText)
        ]
    windowsLayer = vstack [
        hstack [
            filler,
            hstack [mainButton "Apply" (AppCloseWindowConfigScreen True)] 
                `styleBasic` [paddingH 5, paddingV 8],
            hstack [button "Discard" (AppCloseWindowConfigScreen False)] 
                `styleBasic` [paddingH 5, paddingV 8],
            spacer
            ] `styleBasic` [bgColor mainBgColor],
        logWindowsDialogWidget lwModel
        ]
    widgetTree = zstack [
        box_ [] logScreenLayer
            `styleBasic` [bgColor mainBgColor],
        box_ [alignCenter, alignMiddle] colorsLayer
            `nodeVisible` (model^.dialogMode==DMColors)
            `styleBasic` [bgColor (mainBgColor & L.a .~ 0.7)],
        box_ [alignCenter, alignMiddle] windowsLayer
            `nodeVisible` (model^.dialogMode==DMWindows)
            `styleBasic` [bgColor (mainBgColor & L.a .~ 0.7)],
        box_ [alignCenter, alignMiddle] reformattingOverlay
            `nodeVisible` not (null (model^.reforMode))
            `styleBasic` [bgColor (mainBgColor & L.a .~ 0.3)],
        errorOverlay `nodeVisible` isJust (model ^. errorMsg)
        ]

handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent
  -> AppModel -> AppEvent -> [EventResponse AppModel AppEvent AppModel ()]
handleEvent wenv _ model evt = case evt of
    AppErrorShow msg                -> [
        Model $ model
            & errorMsg          ?~ msg
        ]
    AppErrorClose                   -> [
        Model $ model 
            & errorMsg          .~ Nothing
        ]
    AppInit                         -> [
        Model $ model 
            & reforMode         .~ inforceReforMode,
        Producer (logProducer (model^.mainConfig) (model^.logFilePath))
        ]
    AppResize _                     -> [
        Model $ model
            & reforMode         .~ inforceReforMode,
        Task $ writeAppWindowSize path winSize
        ]
    AppWindowSizeSaved              -> [
        Model $ model 
            & logMergeMode      .~ LMFull,
        Task endReformatting
        ]
    AppReformattingDone             -> [
        Model $ model 
            & logMergeMode      .~ (if isReforMode then LMFull else LMLast)
            & reforMode         .~ relaxReforMode
        ]
    AppShowColorConfig              -> [
        Model $ model 
            & dialogMode        .~ DMColors
            & cModel.cDistrib   .~ model^.logColorDistrib
        ]
    AppCloseColorConfigScreen True  -> [
        Model $ model 
            & dialogMode        .~ DMNone
            & reforMode         .~ inforceReforMode,
        Task $ saveColorConfig path (model^.cModel.cDistrib)
        ]
    AppCloseColorConfigScreen False -> [
        Model $ model 
            & dialogMode        .~ DMNone
        ]
    AppColorConfigSaved             -> [
        Model $ model 
            & logMergeMode      .~ LMFull
            & logColorDistrib   .~ model^.cModel.cDistrib,
        Task endReformatting
        ]
    AppShowWindowConfig             -> [
        Model $ model 
            & dialogMode        .~ DMWindows
            & lwModel.lwLists   .~ map (\i -> 
                M.keys (M.filter (==i) (model^.logWindowDistrib))
                ) [0..(model^.lwModel.lwLists.to length)-1]
        ]
    AppCloseWindowConfigScreen True -> [
        Model $ model 
            & dialogMode        .~ DMNone
            & reforMode         .~ inforceReforMode,
        Task $ saveWindowConfig path newLogWindowDistrib
        ]
    AppCloseWindowConfigScreen False -> [
        Model $ model 
            & dialogMode        .~ DMNone
        ]
    AppWindowConfigSaved -> [
        Model $ model 
            & logMergeMode      .~ LMFull
            & logWindowDistrib  .~ newLogWindowDistrib,
        Task endReformatting
        ]
    AppAddRecord le                 -> [
        Model $ model
            & logMergeMode      .~ 
                (if isLogEntryVisible le then LMLast else LMNo)
            & curTime           .~ fromMaybe (model^.curTime) (le^.leTime)
            & logEntries        .~ addToLogEntriesDepository 
                (cfg^. acMaximumLogEntries) le (model^.logEntries),
        Task $ afterAddRecord (showt (le^.leId))
        ]
    AppSetFocus wKey -> [
        SetFocusOnKey (WidgetKey wKey),
        Message (WidgetKey wKey) AnimationStart
        ]
    AppAddBulkRecords les leid      -> [
        Model $ model
            & curTime           .~ model^.curTime
            & logEntries        .~ makeLogEntriesDepositoryFromBulk les,
        SetFocusOnKey (WidgetKey (showt leid))
        ]
    where
        winSize' = wenv^. L.windowSize
        winSize = (round $ winSize'^. L.w, round $ winSize'^. L.h)
        calcLogWindowDistrib :: [[LogEntryTag]] -> LogWindowDistrib
        calcLogWindowDistrib lsts  =
            List.foldl' (\xds i ->
                List.foldl' (\xds' k -> M.insert k i xds') xds (lsts!!i)
                ) (model^.logWindowDistrib) [0..length lsts-1]
        newLogWindowDistrib = 
            calcLogWindowDistrib $ model^.lwModel.lwLists
        inforceReforMode = 0:(model^.reforMode)
        relaxReforMode = case model^.reforMode of
            []      -> []
            [_]     -> []
            (_:xs)  -> xs
        isReforMode = length (model^.reforMode)>1
        path = model^. exePath
        cfg = model^. mainConfig
        isLogEntryVisible le =
            (model^.logWindowDistrib) M.! (le^. leReLogEntry.rleTag) > 0

-- *****************************************************************************

-- Get integer seconds between tw0 times
secondsSince :: UTCTime -> UTCTime -> Int
secondsSince t0 t = i
   where (i, _) = properFraction $ diffUTCTime t0 t

-- Log fetcher (from gamelog)
logProducer :: MainConfig -> FilePath -> (AppEvent -> IO ()) -> IO ()
logProducer cfg logpath sendMsg = ( do
        let lpCfg = LogParseConfig 
            sm = symMapping
            reCfg = ReConfig (cfg^. acShowProfession) (cfg^. acShowName)
        f <- openFile logpath ReadMode
        hSetEncoding f latin1
        prevLines <- unfoldM ( do
            eof <- hIsEOF f
            if eof then return Nothing
                   else TIO.hGetLine f <&> Just
            )
        let n = _acPreviousLogEntries cfg
            latestLines = force $ map (textTranslate sm) $ 
                mergeLogBlocks $ 
                latestLogCut n prevLines :: [Text]
            leds = force $ 
                map (reassemble reCfg . parseLogEntry lpCfg) latestLines
            les = force $ reverse $ zipWith (`LogEntry` Nothing) [1..] leds
            lstId = if null les then 0
                     else let LogEntry lastId' _ _  = head les
                          in lastId'
        sendMsg $ AppAddBulkRecords les lstId
        sendMsg AppReformattingDone
        iterateM_ (\leid -> do
            eof <- hIsEOF f
            lineMb <- if eof then return Nothing
                      else Just <$> TIO.hGetLine f
            case lineMb of
                Nothing     -> threadDelay readTimeout >> return leid
                Just line   -> do
                    utcTime <- getCurrentTime
                    sendMsg $ AppAddRecord $ force $ LogEntry leid 
                        (Just utcTime) 
                        (reassemble reCfg $ 
                            parseLogEntry lpCfg (textTranslate sm line))
                    return (leid+1)
            ) (lstId+1)
    ) `catches` 
        [ Handler (\ (ex :: SomeException)  -> sendMsg $ 
                AppErrorShow (T.pack (show ex)))
        ]

-- *****************************************************************************

colorConfigFile :: FilePath
colorConfigFile = "colors"

windowConfigFile :: FilePath
windowConfigFile = "windows"

-- | Add line breaks to list of tuples string 
prettyList :: String -> String
prettyList str0 = 
    concat $ List.unfoldr (\(str', b) -> 
        if b then Nothing
        else case List.elemIndex ')' str' of 
                Just i -> let (tuple, rest) = List.splitAt (i+1) str'
                          in Just (tuple, ('\n':rest, b))
                Nothing -> Just (str', ("", True))
        ) (str0, False)

saveColorConfig :: FilePath -> LogColorDistrib -> IO AppEvent
saveColorConfig path logColors = do
    writeFile (path </> colorConfigFile) (prettyList $ show logColors)
        `catch` \(_::SomeException) -> throw ExSaveColorConfig
    return AppColorConfigSaved

readColorConfig :: FilePath -> IO LogColorDistrib
readColorConfig path = do
    logColors <- catch ( 
        readFile (path </> colorConfigFile)
        ) (\(e::SomeException) -> throw (ExReadColorConfig $ show e))
    return $ fromMaybe 
        (throw (ExReadColorConfig "can't parse"))
        (fmap fst . listToMaybe . reads $ logColors)
        
saveWindowConfig :: FilePath -> LogWindowDistrib -> IO AppEvent
saveWindowConfig path logWindows = do
    writeFile (path </> windowConfigFile) (prettyList $ show logWindows)
        `catch` \(_::SomeException) -> throw ExSaveColorConfig
    return AppWindowConfigSaved

readWindowConfig :: FilePath -> IO LogWindowDistrib
readWindowConfig path = do
    logWindows <- catch ( 
        readFile (path </> windowConfigFile)
        ) (\(e::SomeException) -> throw (ExReadWindowConfig $ show e))
    return $ fromMaybe 
        (throw (ExReadWindowConfig "can't parse"))
        (fmap fst . listToMaybe . reads $ logWindows)

-- *****************************************************************************

type AppWindowSize = (Int, Int)

appWindowSizeFile :: FilePath
appWindowSizeFile = "winsize"

writeAppWindowSize :: FilePath -> AppWindowSize -> IO AppEvent
writeAppWindowSize path aws = do
    writeFile (path </> appWindowSizeFile) (show aws)
        `catch` \(_::SomeException) -> throw ExSaveAppWindowSize
    return AppWindowSizeSaved

readAppWindowSize :: FilePath -> MainConfig -> IO AppWindowSize
readAppWindowSize path cfg =
    catch (
        read <$> readFile (path </> appWindowSizeFile)
        ) (\(_::SomeException) -> return (cfg^. acMainWindowDefaultSize))    -- throw ExReadAppWindowSize
        
-- *****************************************************************************

endReformatting :: IO AppEvent
endReformatting =
    return AppReformattingDone

afterAddRecord :: Text -> IO AppEvent
afterAddRecord wKey =
    return $ AppSetFocus wKey

