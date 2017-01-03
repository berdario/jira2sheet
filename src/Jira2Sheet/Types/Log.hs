{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Jira2Sheet.Types.Log where

import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Log         (LoggingT (..), WithSeverity)
import qualified Control.Monad.Log         as Log
import           Control.Monad.Trans.Class (lift)
import           Data.Text                 (Text)

class (Monad m) => Log m where
     logDebug :: Text -> m ()
     logInfo :: Text -> m ()
     logError :: Text -> m ()

instance (Monad m) => Log (LoggingT (WithSeverity Text) m) where
    logDebug = Log.logDebug
    logInfo = Log.logInfo
    logError = Log.logError

instance (Log m) => Log (ExceptT e m) where
    logDebug = lift . logDebug
    logInfo = lift . logInfo
    logError = lift . logError
