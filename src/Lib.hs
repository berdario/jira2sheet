{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
        jiraApp
      , DomainName(..)
      , MonadHTTPGet
      , WriteFS
      , Log
      , MonadInput
      , query
      , queryAll
      , getCredentials
      , JQL(..)
      , SearchQuery(..)
      , JiraStatus(..)
      , JiraFields(..)
      , JiraIssue(..)
      , JiraResponse(..)
    ) where

import           Prelude                    hiding (writeFile)

import           Control.Lens               ((&), (?~), (^.))
import           Control.Monad              (MonadPlus, mzero, (<=<))
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Log          (LoggingT (..), WithSeverity,
                                             runLoggingT)
import qualified Control.Monad.Log          as Log
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Aeson                 (FromJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BS
import           Data.Csv                   (DefaultOrdered, ToField (toField),
                                             ToNamedRecord,
                                             encodeDefaultOrderedByName)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           GHC.Generics               (Generic)
import           Network.URI                (URI (..), URIAuth (..))
import           Network.Wreq               (Auth, asJSON, auth, defaults,
                                             responseBody)
import qualified Network.Wreq               as Wreq
import           System.Console.Haskeline   (InputT, MonadException (..),
                                             RunIO (..), defaultSettings,
                                             runInputT)
import qualified System.Console.Haskeline   as Haskeline

class Log m where
     logDebug :: Text -> m ()
     logInfo :: Text -> m()

class (Monad m, MonadThrow m) => MonadHTTPGet m where
    getWith :: (FromJSON a) => Wreq.Options -> String -> m (Wreq.Response a)

class (Monad m) => WriteFS m where
    writeFile :: FilePath -> BS.ByteString -> m ()

class (Monad m) => MonadInput m where
    getInputLine :: String -> m String
    getPassword :: Maybe Char -> String -> m String

instance (Monad m) => Log (LoggingT (WithSeverity Text) m) where
    logDebug = Log.logDebug
    logInfo = Log.logInfo

instance MonadHTTPGet IO where
    getWith options = asJSON <=< Wreq.getWith options

instance WriteFS IO where
    writeFile = BS.writeFile

runInput :: (MonadException m, MonadPlus m) => InputT m (Maybe a) -> m a
runInput = maybe mzero return <=< runInputT defaultSettings

instance MonadInput IO where
    getInputLine = runInput . Haskeline.getInputLine
    getPassword c = runInput . Haskeline.getPassword c

instance (MonadHTTPGet m) => MonadHTTPGet (LoggingT message m) where
    getWith options = lift . getWith options

instance (WriteFS m) => WriteFS (LoggingT message m) where
    writeFile path = lift . writeFile path

instance (MonadHTTPGet m) => MonadHTTPGet (MaybeT m) where
    getWith options = lift . getWith options

instance (WriteFS m) => WriteFS (MaybeT m) where
    writeFile path = lift . writeFile path

instance (MonadInput m) => MonadInput (LoggingT message m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

instance (MonadInput m) => MonadInput (MaybeT m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

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
showBS = Text.encodeUtf8 . Text.pack . show

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

getCredentials :: (MonadInput m) => m Auth
getCredentials = do
    user <- getInputLine "Jira username>"
    password <- getPassword Nothing "Jira password>"
    return $ basicAuth user password

jqlQuery = "project %3D RATM AND issueType not in (Epic%2C Sub-task) AND status not in (Done%2C Resolved%2C Backlog%2C \"Selected for Development\")"

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


jiraApp :: (MonadInput m, Log m, MonadHTTPGet m, WriteFS m) =>
            DomainName -> m ()
jiraApp domain = do
    credentials <- getCredentials
    let queryChunk start = query credentials domain (SearchQuery (JQL jqlQuery) start)
    is <- queryAll queryChunk
    writeFile "jira.csv" $ encodeDefaultOrderedByName $ map (jiraToRecord domain) is
    logInfo "jira.csv created"
