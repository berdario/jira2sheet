{-# LANGUAGE FlexibleInstances #-}

module Jira2Sheet.Types.Input where

import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Log         (LoggingT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           System.Console.Haskeline  (InputT, defaultSettings, runInputT)
import qualified System.Console.Haskeline  as Haskeline
import           UnexceptionalIO           (UIO, unsafeFromIO)

class (Monad m) => MonadInput m where
    getInputLine :: String -> m String
    getPassword :: Maybe Char -> String -> m String

getPassword' :: (MonadInput m) => String -> m String
getPassword' = getPassword (Just '*')


runInput :: InputT IO (Maybe a) -> MaybeT UIO a -- TODO add back MonadException, convert to MonadError?
runInput = MaybeT . unsafeFromIO . runInputT defaultSettings

instance MonadInput (MaybeT UIO) where
    getInputLine = runInput . Haskeline.getInputLine
    getPassword c = runInput . Haskeline.getPassword c

instance (MonadInput m) => MonadInput (LoggingT message m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

instance (MonadInput m) => MonadInput (ExceptT e m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c


