{-|
Module      : AppException
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Application exceptions definition
-}
{-# LANGUAGE ExistentialQuantification #-}

module AppException where

import Control.Exception
import Data.Typeable ( cast )

import LogParser.Reassemble ( ReLogEntry )


data AppException = forall e . Exception e => AppException e

instance Show AppException where
    show (AppException e) = show e

instance Exception AppException

appExceptionToException :: Exception e => e -> SomeException
appExceptionToException = toException . AppException

appExceptionFromException :: Exception e => SomeException -> Maybe e
appExceptionFromException x = do
    AppException a <- fromException x
    cast a


newtype ExEmptyComponent = ExEmptyComponent ReLogEntry

instance Show ExEmptyComponent where
    show (ExEmptyComponent reas) = "empty text component: "++show reas

instance Exception ExEmptyComponent where
    toException   = appExceptionToException
    fromException = appExceptionFromException


newtype ExLogWindowsNumber = ExLogWindowsNumber Int

instance Show ExLogWindowsNumber where
    show (ExLogWindowsNumber n) = "invalid number of log windows: "++show n

instance Exception ExLogWindowsNumber where
    toException   = appExceptionToException
    fromException = appExceptionFromException


newtype ExLogColumns = ExLogColumns Int

instance Show ExLogColumns where
    show (ExLogColumns n) = "invalid number of log columns: "++show n

instance Exception ExLogColumns where
    toException   = appExceptionToException
    fromException = appExceptionFromException


newtype ExConfigCheck = ExConfigCheck String

instance Show ExConfigCheck where
    show (ExConfigCheck msg) = "config check fail: "++msg

instance Exception ExConfigCheck where
    toException   = appExceptionToException
    fromException = appExceptionFromException

newtype ExInvalidConfigRecord = ExInvalidConfigRecord String

instance Show ExInvalidConfigRecord where
    show (ExInvalidConfigRecord msg) = " "++msg

instance Exception ExInvalidConfigRecord where
    toException   = appExceptionToException
    fromException = appExceptionFromException


data ExSaveColorConfig = ExSaveColorConfig

instance Show ExSaveColorConfig where
    show ExSaveColorConfig = "unable to save color configuration"

instance Exception ExSaveColorConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExReadColorConfig = ExReadColorConfig

instance Show ExReadColorConfig where
    show ExReadColorConfig = "unable to read color configuration"

instance Exception ExReadColorConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExSaveWindowConfig = ExSaveWindowConfig

instance Show ExSaveWindowConfig where
    show ExSaveWindowConfig = "unable to save log windows configuration"

instance Exception ExSaveWindowConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExReadWindowConfig = ExReadWindowConfig

instance Show ExReadWindowConfig where
    show ExReadWindowConfig = "unable to read log windows configuration"

instance Exception ExReadWindowConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExSaveAppWindowSize = ExSaveAppWindowSize

instance Show ExSaveAppWindowSize where
    show ExSaveAppWindowSize = "unable to save App window size file"

instance Exception ExSaveAppWindowSize where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExReadAppWindowSize = ExReadAppWindowSize

instance Show ExReadAppWindowSize where
    show ExReadAppWindowSize = "unable to read App window size file"

instance Exception ExReadAppWindowSize where
    toException   = appExceptionToException
    fromException = appExceptionFromException


data ExSaveMainConfig = ExSaveMainConfig

instance Show ExSaveMainConfig where
    show ExSaveMainConfig = "unable to save main configuration"

instance Exception ExSaveMainConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExReadMainConfig = ExReadMainConfig

instance Show ExReadMainConfig where
    show ExReadMainConfig = "unable to read main configuration"

instance Exception ExReadMainConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException

data ExDecodeMainConfig = ExDecodeMainConfig String String

instance Show ExDecodeMainConfig where
    show (ExDecodeMainConfig s0 s1) = "unable to decode main configuration: "++s0++": "++s1

instance Exception ExDecodeMainConfig where
    toException   = appExceptionToException
    fromException = appExceptionFromException



