{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Jira2Sheet where

import           Control.Exception       (SomeException)
import           Control.Monad.Except    (MonadError)
import           Data.Monoid             ((<>))
import           Network.HTTP.Conduit    (tlsManagerSettings)

import           Jira2Sheet.Auth         (Credentials (..), getCredentials)
import           Jira2Sheet.Common       (DomainName)
import           Jira2Sheet.Csv          (encodeToCsv)
import           Jira2Sheet.GoogleDrive  (DriveFileMetadata (fileId), oauth,
                                          uploadCsv)
import           Jira2Sheet.Jira         (JQL, jiraQuery)
import           Jira2Sheet.Types.Crypto (MonadCrypto (..))
import           Jira2Sheet.Types.Files  (MonadReadFS (..), MonadWriteFS (..))
import           Jira2Sheet.Types.HTTP   (MonadHTTP (..), MonadHTTPGet (..),
                                          MonadOAuth (..))
import           Jira2Sheet.Types.Input  (MonadInput)
import           Jira2Sheet.Types.Log    (Log (..))


jira2sheet :: (MonadInput m, Log m, MonadHTTPGet m, MonadHTTP m, MonadWriteFS m, MonadReadFS m, MonadOAuth m, MonadCrypto m, MonadError SomeException m) =>
            (DomainName, JQL) -> m ()
jira2sheet (domain, jql) = do
    mgr <- newTls tlsManagerSettings
    Credentials auth tkn <- getCredentials mgr oauth
    issues <- jiraQuery auth domain jql
    fileMetadata <- uploadCsv tkn $ encodeToCsv domain issues
    logInfo $ "created https://drive.google.com/file/d/" <> fileId fileMetadata
