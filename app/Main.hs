{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Exception          (SomeException)
import           Control.Monad              (join, void)
import           Control.Monad.Log          (LoggingT, Severity (Informational),
                                             WithSeverity, discardSeverity,
                                             msgSeverity, runLoggingT)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Text                  (Text)
import qualified Data.Text.IO               as Text
import           Options.Applicative        (Parser, execParser, fullDesc, help,
                                             info, long, metavar, option,
                                             optional, short, str, (<>))
import           UnexceptionalIO            (UIO, runEitherIO, unsafeFromIO)


import           Jira2Sheet.Common          (DomainName (..))
import           Jira2Sheet.Jira            (JQL (..))
import           Lib


maxLogLevel :: Severity
maxLogLevel = Informational

logHandler :: (MonadTrans t) => WithSeverity Text -> t (MaybeT UIO) ()
logHandler msg = lift $ case compare (msgSeverity msg) maxLogLevel of
    GT -> return ()
    _  -> lift $ unsafeFromIO $ Text.putStrLn $ discardSeverity msg


domainOption = option str
    ( long "domain"
    <> short 'd'
    <> metavar "<Jira domain name>"
    <> help "The domain name of the Jira instance" )

projectOption = option str
    ( long "project"
    <> short 'p'
    <> metavar "<Project>"
    <> help "The project name on which to filter the issues")

teamOption = optional $ option str
    ( long "team"
    <> short 't'
    <> metavar "<Team>"
    <> help "The optional team name on which to filter the issues")

parser :: Parser (DomainName, JQL)
parser = (,) <$> (DomainName <$> domainOption) <*> (JQL <$> projectOption <*> teamOption)

runner :: LoggingT (WithSeverity Text) (ExceptT SomeException (MaybeT UIO)) a -> IO (Maybe a)
runner = runEitherIO . fmap sequence . runMaybeT . runExceptT . flip runLoggingT logHandler

main :: IO ()
main = do
    args <- execParser (info parser fullDesc)
    void $ runner $ jiraApp args
