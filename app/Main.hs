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
import           Options.Applicative        (Parser, argument, execParser,
                                             fullDesc, info, metavar, str)
import           UnexceptionalIO            (UIO, runEitherIO, unsafeFromIO)


import           Lib


maxLogLevel :: Severity
maxLogLevel = Informational

logHandler :: (MonadTrans t) => WithSeverity Text -> t (MaybeT UIO) ()
logHandler msg = lift $ case compare (msgSeverity msg) maxLogLevel of
    GT -> return ()
    _  -> lift $ unsafeFromIO $ Text.putStrLn $ discardSeverity msg


parser :: Parser String
parser = argument str (metavar "<Jira domain name>")

runner :: LoggingT (WithSeverity Text) (ExceptT SomeException (MaybeT UIO)) a -> IO (Maybe a)
runner = runEitherIO . fmap sequence . runMaybeT . runExceptT . flip runLoggingT logHandler

main :: IO ()
main = do
    domain <- execParser (info parser fullDesc)
    void $ runner $ jiraApp $ DomainName domain
