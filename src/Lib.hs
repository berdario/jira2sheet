{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Lib
    (
        jiraApp
      , DomainName(..)
      , MonadHTTPGet
      , MonadWriteFS
      , MonadReadFS
      , Log
      , MonadInput
      , MonadOAuth
      , query
      , queryAll
      , getCredentials
      , JQL(..)
      , SearchQuery(..)
      , JiraStatus(..)
      , JiraFields(..)
      , JiraIssue(..)
      , JiraResponse(..)
      , Credentials(..)
      , SavedCredentials(..)
    ) where

import           Prelude                    hiding (writeFile)

import           Control.Exception          (Exception, SomeException (..))
import           Control.Lens               ((&), (?~), (^.))
import           Control.Monad              (MonadPlus, join, mzero, unless,
                                             (<=<))
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Except       (ExceptT (..), MonadError (..),
                                             mapExceptT, runExceptT)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Log          (LoggingT (..), WithSeverity,
                                             runLoggingT)
import qualified Control.Monad.Log          as Log
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..), mapMaybeT,
                                             maybeToExceptT)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Aeson                 (FromJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BS
import           Data.Csv                   (DefaultOrdered, ToField (toField),
                                             ToNamedRecord,
                                             encodeDefaultOrderedByName)
import           Data.Either.Combinators    (mapLeft)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Encoding    as LazyText
import           GHC.Generics               (Generic)
import           Network.HTTP.Conduit       (Manager, ManagerSettings,
                                             newManager, tlsManagerSettings)
import           Network.OAuth.OAuth2       (AccessToken (..), OAuth2 (..),
                                             QueryParams, appendQueryParam,
                                             authorizationUrl, fetchAccessToken)
import qualified Network.OAuth.OAuth2       as OAuth2
import           Network.URI                (URI (..), URIAuth (..))
import           Network.Wreq               (Auth, asJSON, auth, defaults,
                                             responseBody)
import qualified Network.Wreq               as Wreq
import           System.Console.Haskeline   (InputT, MonadException (..),
                                             RunIO (..), defaultSettings,
                                             runInputT)
import qualified System.Console.Haskeline   as Haskeline
import           UnexceptionalIO            (UIO, fromIO, unsafeFromIO)
import           Web.Browser                (openBrowser)

class (Monad m) => Log m where
     logDebug :: Text -> m ()
     logInfo :: Text -> m()

data Error = Error String deriving (Show, Eq)
deriving instance Exception Error

class (Monad m ) => MonadHTTPGet m where -- TODO add back MonadThrow, change to MonadError
    getWith :: (FromJSON a) => Wreq.Options -> String -> m (Wreq.Response a)

class (Monad m) => MonadWriteFS m where
    writeFile :: FilePath -> BS.ByteString -> m ()

class (Monad m) => MonadReadFS m where
    decryptSavedCredentials :: FilePath -> ByteString -> MaybeT m SavedCredentials

class (Monad m) => MonadInput m where
    getInputLine :: String -> m String
    getPassword :: Maybe Char -> String -> m String

class (Monad m) => MonadOAuth m where
    newTls :: ManagerSettings -> m Manager
    oauthAuthorize :: Manager -> OAuth2 -> QueryParams -> m AccessToken
    fetchRefreshToken :: Manager -> OAuth2 -> ByteString -> m AccessToken

instance (Monad m) => Log (LoggingT (WithSeverity Text) m) where
    logDebug = Log.logDebug
    logInfo = Log.logInfo

instance (Log m) => Log (ExceptT e m) where
    logDebug = lift . logDebug
    logInfo = lift . logInfo

instance MonadHTTPGet UIO where
    getWith options = unsafeFromIO . (asJSON <=< Wreq.getWith options)

instance MonadWriteFS UIO where
    writeFile path = unsafeFromIO . BS.writeFile path

instance MonadReadFS UIO where
    decryptSavedCredentials fp key = mzero -- TODO

runInput :: InputT IO (Maybe a) -> MaybeT UIO a -- TODO add back MonadException, convert to MonadError?
runInput = MaybeT . unsafeFromIO . runInputT defaultSettings

instance MonadInput (MaybeT UIO) where
    getInputLine = runInput . Haskeline.getInputLine
    getPassword c = runInput . Haskeline.getPassword c

encode :: String -> ByteString
encode = Text.encodeUtf8 . Text.pack

decode :: ByteString -> String
decode = Text.unpack . Text.decodeUtf8

lazyDecode :: BS.ByteString -> String
lazyDecode = LazyText.unpack . LazyText.decodeUtf8

oauthToExcept :: IO (Either BS.ByteString a) -> ExceptT SomeException (MaybeT UIO) a
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

instance (MonadHTTPGet m) => MonadHTTPGet (ExceptT e m) where
    getWith options = lift . getWith options

instance (MonadWriteFS m) => MonadWriteFS (LoggingT message m) where
    writeFile path = lift . writeFile path

instance (MonadWriteFS m) => MonadWriteFS (ExceptT e m) where
    writeFile path = lift . writeFile path

instance (MonadReadFS m) => MonadReadFS (LoggingT message m) where
    decryptSavedCredentials fp = mapMaybeT lift . decryptSavedCredentials fp

instance (MonadReadFS m) => MonadReadFS (ExceptT e m) where
    decryptSavedCredentials fp = mapMaybeT lift . decryptSavedCredentials fp

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

instance (MonadWriteFS m) => MonadWriteFS (MaybeT m) where
    writeFile path = lift . writeFile path

instance (MonadReadFS m) => MonadReadFS (MaybeT m) where
    decryptSavedCredentials fp = mapMaybeT lift . decryptSavedCredentials fp

instance (MonadInput m) => MonadInput (LoggingT message m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

instance (MonadInput m) => MonadInput (ExceptT e m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

type RefreshToken = ByteString
data SavedCredentials = SavedCredentials Auth RefreshToken
data Credentials = Credentials Auth AccessToken

data JiraStatus = JiraStatus {
    name :: Text
} deriving (Show, Eq, Generic, FromJSON)

data JiraFields = JiraFields {
    status            :: JiraStatus
  , summary           :: Text
  , customfield_10007 :: Maybe Text -- Epic
} deriving (Show, Eq, Generic, FromJSON)

data JiraIssue = JiraIssue {
    fields :: JiraFields
  , key    :: Text
} deriving (Show, Eq, Generic, FromJSON)

data JiraResponse = JiraResponse {
    maxResults :: Int
  , total      :: Int
  , issues     :: [JiraIssue]
} deriving (Show, Eq, Generic, FromJSON)

data Degree = L | M | H deriving (Eq, Ord, Show)

data Issue = Issue {
    num            :: Maybe Int
  , epic           :: Maybe Text
  , stakeholder    :: Text
  , shortname      :: Text
  , fullname       :: Text
  , pm             :: Text
  , comment        :: Text
  , jira           :: URI
  , size           :: Maybe Int
  , team           :: Text
  , progressstatus :: Text
  , abtest         :: Text
  , priority       :: Maybe Degree
  , impact         :: Maybe Degree
  , impactedKPI    :: Text
  , islift         :: Maybe Bool
} deriving (Show, Eq, Generic)

instance ToField Bool where
    toField True  = "Yes"
    toField False = "No"

showBS :: (Show a) => a -> ByteString
showBS = encode . show

instance ToField URI where
    toField = showBS

instance ToField Degree where
    toField = showBS

instance ToNamedRecord Issue
instance DefaultOrdered Issue

jiraToRecord :: DomainName -> JiraIssue -> Issue
jiraToRecord domain JiraIssue{
    key=jiraKey,
    fields=JiraFields{ summary=title
                     , customfield_10007=epicKey
                     , status=JiraStatus{name=jiraStatus}}
} = Issue {
    num            = Nothing
  , epic           = epicKey
  , stakeholder    = ""
  , shortname      = ""
  , fullname       = title
  , pm             = ""
  , comment        = ""
  , jira           = jiraBrowse domain (Text.unpack jiraKey)
  , size           = Nothing
  , team           = ""
  , progressstatus = jiraStatus
  , abtest         = ""
  , priority       = Nothing
  , impact         = Nothing
  , impactedKPI    = ""
  , islift         = Nothing
}

basicAuth :: String -> String -> Auth
basicAuth user password = Wreq.basicAuth (encode user) (encode password)
    where encode = Text.encodeUtf8 . Text.pack

getJiraCredentials :: (MonadInput m) => m Auth
getJiraCredentials = do
    user <- getInputLine "Jira username>"
    password <- getPassword Nothing "Jira password>"
    return $ basicAuth user password

saveCredentials :: MonadWriteFS m => SavedCredentials -> m ()
saveCredentials _ = pure () -- writeFile "credentials.enc" undefined -- TODO

authorizeAndSave :: (MonadOAuth m, MonadWriteFS m) => Manager -> OAuth2 -> Auth -> m AccessToken
authorizeAndSave mgr oauth jCredentials = do
    googleRefresh <- oauthAuthorize mgr oauth [("scope", "https://www.googleapis.com/auth/drive")]
    traverse (saveCredentials . SavedCredentials jCredentials) $ refreshToken googleRefresh
    pure googleRefresh

getCredentials :: (MonadInput m, MonadReadFS m, MonadOAuth m, MonadWriteFS m) => Manager -> OAuth2 -> m Credentials
getCredentials mgr oauth = do
    credentials <- runMaybeT $ decryptSavedCredentials undefined undefined
    let authorizeAndSave' = authorizeAndSave mgr oauth
    case credentials of
        Nothing -> do
            jCredentials <- getJiraCredentials
            Credentials jCredentials <$> authorizeAndSave' jCredentials
        (Just (SavedCredentials jCred tkn)) ->
            Credentials jCred <$> fetchRefreshToken mgr oauth tkn
            -- tokenResponse <- runExceptT $ fetchRefreshToken mgr oauth tkn
            -- Credentials jCred <$> either (const $ authorizeAndSave' jCred) pure tokenResponse


jqlQuery = "project %3D RATM AND issueType not in (Epic%2C Sub-task) AND status not in (Done%2C Resolved%2C Backlog%2C \"Selected for Development\")"
-- jqlQuery = "project %3D DEV AND \"Assigned Team\" %3D \"Dire Straits\" AND issueType not in (Epic%2C Sub-task) AND status not in (Done%2C Resolved%2C Backlog%2C \"Selected for Development\")"

newtype DomainName = DomainName String
newtype JQL = JQL String

data SearchQuery = SearchQuery
    JQL
    Int -- startAt

jiraBrowse (DomainName domain) jiraKey =
    URI { uriScheme="https:"
        , uriAuthority=Just URIAuth { uriUserInfo=""
                                    , uriRegName=domain
                                    , uriPort=""}
        , uriPath="/browse/" <> jiraKey
        , uriQuery="", uriFragment=""}

jiraURI :: DomainName -> SearchQuery -> URI
jiraURI (DomainName domain) (SearchQuery (JQL jql) start) =
    URI { uriScheme="https:"
        , uriAuthority=Just URIAuth { uriUserInfo=""
                                    , uriRegName=domain
                                    , uriPort=""}
        , uriPath="/rest/api/latest/search"
        , uriQuery="?jql=" <> jql <> "&startAt=" <> show start
        , uriFragment=""}

query :: (MonadHTTPGet m) => Auth -> DomainName -> SearchQuery -> m JiraResponse
query credentials domain query = do
    let opts = defaults & auth ?~ credentials
    response <- getWith opts (show $ jiraURI domain query)
    return $ response ^. responseBody

queryAll :: (Log m, MonadHTTPGet m) => (Int -> m JiraResponse) -> m [JiraIssue]
queryAll query = do
    response <- query 0
    let is = issues response
    let maxNumber = maxResults response
    case compare (total response) maxNumber of
        GT -> queryRest query [] is (length is)
        _  -> return is

queryRest :: (Log m, MonadHTTPGet m) => (Int -> m JiraResponse) -> [JiraIssue] -> [JiraIssue] -> Int -> m [JiraIssue]
queryRest _ acc [] _ = return acc
queryRest query acc is count = do
    logDebug "Results exceeded maxResults, requesting next page..."
    is' <- issues <$> query count
    queryRest query (acc ++ is) is' (count + length is')


jiraApp :: (MonadInput m, Log m, MonadHTTPGet m, MonadWriteFS m, MonadReadFS m, MonadOAuth m) =>
            DomainName -> m ()
jiraApp domain = do
    mgr <- newTls tlsManagerSettings
    Credentials auth tkn <- getCredentials mgr oauth
    let queryChunk start = query auth domain (SearchQuery (JQL jqlQuery) start)
    is <- queryAll queryChunk
    writeFile "jira.csv" $ encodeDefaultOrderedByName $ map (jiraToRecord domain) is
    logInfo "jira.csv created"

oauth = OAuth2 { oauthClientId = "804038769221-786vn5l5m772h21masc5p4nm3gl995as.apps.googleusercontent.com"
               , oauthClientSecret = "0JuYNS6p7ibK8jA38_rJBkWO"
               , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
               , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
               , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v4/token" }
