{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Helery.Logger
    ( mkLogger
    , debugLog
    , infoLog
    , warnLog
    , criticalLog
    , logStr
    )where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (lift)
import           Katip                  (ColorStrategy (ColorIfTerminal),
                                         LogEnv, LogStr, Severity (..),
                                         Verbosity (V0), initLogEnv, logStr,
                                         logTM, mkHandleScribe, registerScribe,
                                         runKatipContextT)

import           System.IO              (stdout)

-- ----------------------------------------------------------------------------
-- Export Functions
-- ----------------------------------------------------------------------------

-- | create logger
mkLogger :: IO LogEnv
mkLogger = do
    logEnv <- initLogEnv "Helery" "prod"
    defaultScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V0
    return $ registerScribe "default stdout scribe" defaultScribe logEnv

-- | logging for debug level
debugLog logger = baseLog logger DebugS

-- | logging for info level
infoLog logger = baseLog logger InfoS

-- | logging for warning level
warnLog logger = baseLog logger WarningS

-- | logging for critical level
criticalLog logger = baseLog logger CriticalS

-- ----------------------------------------------------------------------------

baseLog :: MonadIO m
           =>LogEnv
           -> Severity
           -> LogStr
           -> m a
           -> m a
baseLog logger serverity msg f = runKatipContextT logger () "main" $ do
    $logTM serverity msg
    lift f
