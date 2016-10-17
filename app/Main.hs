{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Log         (LoggingT, Severity (Informational),
                                            WithSeverity, discardSeverity,
                                            msgSeverity, runLoggingT)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Text                 (Text)
import qualified Data.Text.IO              as Text
import           Options.Applicative       (Parser, argument, execParser,
                                            fullDesc, info, metavar, str)


import           Lib


maxLogLevel :: Severity
maxLogLevel = Informational

logHandler :: (MonadIO m) => WithSeverity Text -> m ()
logHandler msg = case compare (msgSeverity msg) maxLogLevel of
    GT -> return ()
    _  -> liftIO $ Text.putStrLn $ discardSeverity msg


parser :: Parser String
parser = argument str (metavar "<Jira domain name>")

runner :: LoggingT (WithSeverity Text) (MaybeT IO) a -> IO (Maybe a)
runner = runMaybeT . flip runLoggingT logHandler

main :: IO ()
main = do
    domain <- execParser (info parser fullDesc)
    void $ runner $ jiraApp $ DomainName domain
