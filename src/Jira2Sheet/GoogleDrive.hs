{-# LANGUAGE OverloadedStrings #-}

module Jira2Sheet.GoogleDrive where

import           Control.Applicative   (empty)
import           Control.Lens          ((&), (.~), (?~), (^.))
import           Data.Aeson            (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types      (typeMismatch)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import           Network.OAuth.OAuth2  (AccessToken (..), OAuth2 (..))
import           Network.Wreq          (auth, defaults, header, param,
                                        responseBody)
import qualified Network.Wreq          as Wreq

import           Jira2Sheet.Types.HTTP (MonadHTTP (..))

data DriveFileMetadata = DriveFileMetadata {
    fileId :: Text
} deriving (Show, Eq)

instance FromJSON DriveFileMetadata where
    parseJSON (Object v) = DriveFileMetadata <$> v .: "id"
    parseJSON invalid    = typeMismatch "DriveFileMetadata" invalid


oauth = OAuth2 { oauthClientId = "804038769221-786vn5l5m772h21masc5p4nm3gl995as.apps.googleusercontent.com"
               , oauthClientSecret = "0JuYNS6p7ibK8jA38_rJBkWO"
               , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
               , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
               , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v4/token" }


uploadCsv :: (MonadHTTP m) => AccessToken -> LBS.ByteString -> m DriveFileMetadata
uploadCsv tkn content = do
    let opts = defaults & auth ?~ Wreq.oauth2Bearer (accessToken tkn)
                        & header "Content-Type" .~ ["text/csv"]
                        & param "uploadType" .~ ["media"]
    response <- postWith opts "https://www.googleapis.com/upload/drive/v3/files" content
    pure $ response ^. responseBody
