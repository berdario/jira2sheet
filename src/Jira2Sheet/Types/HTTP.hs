{-# LANGUAGE FlexibleInstances #-}

module Jira2Sheet.Types.HTTP where

import           Control.Exception         (SomeException (..))
import           Control.Monad             (join, unless, (<=<))
import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Log         (LoggingT (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Aeson                (FromJSON (..))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Either.Combinators   (mapLeft)
import           Data.Monoid               ((<>))
import           Network.HTTP.Conduit      (Manager, ManagerSettings,
                                            newManager)
import           Network.OAuth.OAuth2      (AccessToken (..), OAuth2 (..),
                                            QueryParams, appendQueryParam,
                                            authorizationUrl, fetchAccessToken)
import qualified Network.OAuth.OAuth2      as OAuth2
import           Network.Wreq              (asJSON)
import qualified Network.Wreq              as Wreq
import           Network.Wreq.Types        (Postable)
import           UnexceptionalIO           (UIO, fromIO, unsafeFromIO)
import           Web.Browser               (openBrowser)

import           Jira2Sheet.Common         (Error (..), decode, encode,
                                            lazyDecode)
import           Jira2Sheet.Types.Input    (MonadInput (..))

class (Monad m ) => MonadHTTPGet m where -- TODO add back MonadThrow, change to MonadError
    getWith :: (FromJSON a) => Wreq.Options -> String -> m (Wreq.Response a)

class (MonadHTTPGet m) => MonadHTTP m where
    postWith :: (Postable a, FromJSON b) => Wreq.Options -> String -> a -> m (Wreq.Response b)


class (Monad m) => MonadOAuth m where
    newTls :: ManagerSettings -> m Manager
    oauthAuthorize :: Manager -> OAuth2 -> QueryParams -> m AccessToken
    fetchRefreshToken :: Manager -> OAuth2 -> ByteString -> m AccessToken


instance MonadHTTPGet UIO where
    getWith options = unsafeFromIO . (asJSON <=< Wreq.getWith options)

instance MonadHTTP UIO where
    postWith options body = unsafeFromIO . (asJSON <=< Wreq.postWith options body)


oauthToExcept :: IO (Either LBS.ByteString a) -> ExceptT SomeException (MaybeT UIO) a
oauthToExcept = ExceptT . lift . fmap join . fromIO . fmap ( mapLeft (SomeException . Error . lazyDecode))

instance MonadOAuth (ExceptT SomeException (MaybeT UIO)) where
    newTls = ExceptT . lift . fromIO . newManager

    oauthAuthorize mgr oauth params = do
        let url = decode $ authorizationUrl oauth `appendQueryParam` params
        opened <- lift $ lift $ unsafeFromIO $ openBrowser url
        unless opened $ lift $ lift $ unsafeFromIO $ putStrLn $ "Unable to open the browser for you, please visit: " <> url
        code <- lift $ getInputLine "Paste here the code obtained from the browser>"
        --code <- maybeToExceptT (SomeException $ Error "No code from user") $ getInputLine "Paste here the code obtained from the browser"
        oauthToExcept $ fetchAccessToken mgr oauth $ encode code

    fetchRefreshToken mgr oauth = oauthToExcept . OAuth2.fetchRefreshToken mgr oauth

instance (MonadHTTPGet m) => MonadHTTPGet (LoggingT message m) where
    getWith options = lift . getWith options

instance (MonadHTTP m) => MonadHTTP (LoggingT message m) where
    postWith options body = lift . postWith options body

instance (MonadHTTPGet m) => MonadHTTPGet (ExceptT e m) where
    getWith options = lift . getWith options

instance (MonadHTTP m) => MonadHTTP (ExceptT e m) where
    postWith options body = lift . postWith options body


instance (MonadOAuth m) => MonadOAuth (LoggingT message m) where
    newTls = lift . newTls
    oauthAuthorize mgr oauth = lift . oauthAuthorize mgr oauth
    fetchRefreshToken mgr oauth = lift . fetchRefreshToken mgr oauth

instance (MonadOAuth m) => MonadOAuth (MaybeT m) where
    newTls = lift . newTls
    oauthAuthorize mgr oauth = lift . oauthAuthorize mgr oauth
    fetchRefreshToken mgr oauth = lift . fetchRefreshToken mgr oauth

instance (MonadHTTPGet m) => MonadHTTPGet (MaybeT m) where
    getWith options = lift . getWith options

instance (MonadHTTP m) => MonadHTTP (MaybeT m) where
    postWith options body = lift . postWith options body
