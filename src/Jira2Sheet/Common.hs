{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Jira2Sheet.Common where

import           Control.Exception       (Exception, SomeException (..))
import           Control.Monad.Except    (MonadError (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

newtype DomainName = DomainName String

type RefreshToken = ByteString
type JiraUsername = String
type JiraPassword = String
data SavedCredentials = SavedCredentials JiraUsername JiraPassword RefreshToken deriving (Show, Read)

data EncryptedData = EncryptedData ByteString ByteString ByteString deriving (Show, Read)

data Error = Error String deriving (Show, Eq)
deriving instance Exception Error

throwMessage :: (MonadError SomeException m) => String -> m a
throwMessage = throwError . SomeException . Error

encode :: String -> ByteString
encode = Text.encodeUtf8 . Text.pack

decode :: ByteString -> String
decode = Text.unpack . Text.decodeUtf8

lazyEncode :: String -> LBS.ByteString
lazyEncode = LazyText.encodeUtf8 . LazyText.pack

lazyDecode :: LBS.ByteString -> String
lazyDecode = LazyText.unpack . LazyText.decodeUtf8

