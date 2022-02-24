{-|
Module      : LogParser.LogException
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Log parsing exceptions definition
-}
{-# LANGUAGE ExistentialQuantification #-}

module LogException where

import Control.Exception
import Data.Typeable ( cast )


data LogException = forall e . Exception e => LogException e

instance Show LogException where
    show (LogException e) = show e

instance Exception LogException

logExceptionToException :: Exception e => e -> SomeException
logExceptionToException = toException . LogException

logExceptionFromException :: Exception e => SomeException -> Maybe e
logExceptionFromException x = do
    LogException a <- fromException x
    cast a


