{-|
Module      : LogParser.Rules.Helpers
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Helpers for parsing rules
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module LogParser.Rules.Helpers where

import Control.Exception ( Exception, toException, fromException )
import Control.Lens ( Identity )
import Data.Text ( pack, unpack, Text )
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

pAny :: (Stream Text Identity t) => ParsecT Text u Identity Text
pAny = pMany anyChar

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
    ss <- many1 (noneOf [' '])
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
    s <- manyTill anyChar
            (try (lookAhead (choice 
                (map (try . string) endWith)
            )))
    return $ pack (if null s then s else init s)

pSomething :: [String] -> Parsec Text LogParseConfig Text
pSomething = pSomeone

-- ****************************************************************************

newtype ExLogParse = ExLogParse ParseError

instance Show ExLogParse where
    show (ExLogParse err) = "parsing fail: "++show err

instance Exception ExLogParse where
    toException   = logExceptionToException
    fromException = logExceptionFromException
