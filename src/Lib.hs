{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
        jiraApp
      , DomainName(..)
    ) where

import           Prelude                   hiding (writeFile)

import           Control.Lens              ((&), (?~), (^.))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Log         (MonadLog (..), WithSeverity,
                                            logDebug, logInfo)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Aeson                (FromJSON)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (writeFile)
import           Data.Csv                  (DefaultOrdered, ToField (toField),
                                            ToNamedRecord,
                                            encodeDefaultOrderedByName)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           GHC.Generics              (Generic)
import           Network.URI               (URI (..), URIAuth (..))
import           Network.Wreq              (Auth, asJSON, auth, defaults,
                                            getWith, responseBody)
import qualified Network.Wreq              as Wreq
import           System.Console.Haskeline  (defaultSettings, getInputLine,
                                            getPassword, runInputT)

type Log = MonadLog (WithSeverity Text)

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

showBS :: (Show a) => a -> BS.ByteString
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

runInput = MaybeT . runInputT defaultSettings

getCredentials :: MaybeT IO Auth
getCredentials = do
    user <- runInput $ getInputLine "Jira username>"
    password <- runInput $ getPassword Nothing "Jira password>"
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

query :: Auth -> DomainName -> SearchQuery -> IO JiraResponse
query credentials domain query = do
    let opts = defaults & auth ?~ credentials
    response <- asJSON =<< getWith opts (show $ jiraURI domain query)
    return $ response ^. responseBody

queryAll :: (Log m, MonadIO m) => (Int -> IO JiraResponse) -> m [JiraIssue]
queryAll query = do
    response <- liftIO $ query 0
    let is = issues response
    let maxNumber = maxResults response
    case compare (total response) maxNumber of
        GT -> queryRest query [] is (length is)
        _  -> return is

queryRest :: (Log m, MonadIO m) => (Int -> IO JiraResponse) -> [JiraIssue] -> [JiraIssue] -> Int -> m [JiraIssue]
queryRest _ acc [] _ = return acc
queryRest query acc is count = do
    logDebug "Results exceeded maxResults, requesting next page..."
    is' <- liftIO $ issues <$> query count
    queryRest query (acc ++ is) is' (count + length is')

jiraApp :: (MonadIO (m (MaybeT IO)), MonadTrans m, Log (m (MaybeT IO))) =>
     DomainName -> m (MaybeT IO) ()
jiraApp domain = do
    credentials <- lift getCredentials
    let queryChunk start = query credentials domain (SearchQuery (JQL jqlQuery) start)
    is <- queryAll queryChunk
    liftIO $ writeFile "jira.csv" $ encodeDefaultOrderedByName $ map (jiraToRecord domain) is
    logInfo "jira.csv created"
