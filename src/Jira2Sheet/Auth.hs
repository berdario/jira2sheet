{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Jira2Sheet.Auth where

import           Control.Exception       (SomeException (..))
import           Control.Monad.Except    (MonadError (..))
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Network.HTTP.Conduit    (Manager)
import           Network.OAuth.OAuth2    (AccessToken (..), OAuth2 (..))
import           Network.Wreq            (Auth)
import qualified Network.Wreq            as Wreq

import           Jira2Sheet.Common       (JiraPassword, JiraUsername,
                                          RefreshToken, SavedCredentials (..))
import           Jira2Sheet.Crypto       (decryptSavedCredentials,
                                          saveCredentials)
import           Jira2Sheet.Types.Crypto (MonadCrypto (..))
import           Jira2Sheet.Types.Files  (MonadReadFS (..), MonadWriteFS (..))
import           Jira2Sheet.Types.HTTP   (MonadOAuth (..))
import           Jira2Sheet.Types.Input  (MonadInput (..), getPassword')
import           Jira2Sheet.Types.Log    (Log (..))



data Credentials = Credentials Auth AccessToken

basicAuth :: String -> String -> Auth
basicAuth user password = Wreq.basicAuth (encode user) (encode password)
    where encode = Text.encodeUtf8 . Text.pack

getJiraCredentials :: (MonadInput m) => m (JiraUsername, JiraPassword)
getJiraCredentials = do
    user <- getInputLine "Jira username>"
    password <- getPassword' "Jira password>"
    return (user, password)

authorizeAndSave :: (MonadOAuth m, MonadWriteFS m, MonadInput m, MonadCrypto m, MonadError SomeException m) => Manager -> OAuth2 -> (RefreshToken -> SavedCredentials) -> m AccessToken
authorizeAndSave mgr oauth builder = do
    googleRefresh <- oauthAuthorize mgr oauth [("scope", "https://www.googleapis.com/auth/drive")]
    traverse (saveCredentials . builder) $ refreshToken googleRefresh
    pure googleRefresh

getCredentials :: (MonadInput m, MonadReadFS m, MonadOAuth m, MonadWriteFS m, MonadCrypto m, MonadError SomeException m, Log m) => Manager -> OAuth2 -> m Credentials
getCredentials mgr oauth = do
    credentials <- decryptSavedCredentials "credentials.enc" -- TODO `mplus` pure Nothing
    let authorizeAndSave' = authorizeAndSave mgr oauth
    case credentials of
        Nothing -> do
            (user, pass) <- getJiraCredentials
            Credentials (basicAuth user pass) <$> authorizeAndSave' (SavedCredentials user pass)
        (Just (SavedCredentials user pass tkn)) -> do
            logInfo "credentials decrypted"
            Credentials (basicAuth user pass) <$> fetchRefreshToken mgr oauth tkn
            -- tokenResponse <- runExceptT $ fetchRefreshToken mgr oauth tkn
            -- Credentials jCred <$> either (const $ authorizeAndSave' jCred) pure tokenResponse
