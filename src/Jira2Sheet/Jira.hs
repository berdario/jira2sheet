{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Jira2Sheet.Jira where

import           Control.Lens           ((&), (?~), (^.))
import           Data.Aeson             (FromJSON (..))
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Network.HTTP.Types.URI (urlEncode)
import           Network.URI            (URI (..), URIAuth (..))
import           Network.Wreq           (Auth, auth, defaults, responseBody)

import           Jira2Sheet.Common      (DomainName (..), decode, encode)
import           Jira2Sheet.Types.HTTP  (MonadHTTPGet (..))
import           Jira2Sheet.Types.Log   (Log (..))

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

data JQL = JQL String (Maybe String)

prepareJQL (JQL projectName mTeamName) = encode $ maybe "" (\name -> "\"Assigned Team\" = \"" <> name <> "\" AND ") mTeamName <>
        "project = \"" <> projectName <> "\" AND issueType not in (Epic, Sub-task) AND status not in (Done, Resolved, Backlog, \"Selected for Development\")"

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
jiraURI (DomainName domain) (SearchQuery jql start) =
    URI { uriScheme="https:"
        , uriAuthority=Just URIAuth { uriUserInfo=""
                                    , uriRegName=domain
                                    , uriPort=""}
        , uriPath="/rest/api/latest/search"
        , uriQuery="?jql=" <> decode (urlEncode False (prepareJQL jql)) <> "&startAt=" <> show start
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

jiraQuery :: (Log m, MonadHTTPGet m) => Auth -> DomainName -> JQL -> m [JiraIssue]
jiraQuery auth domain jql =
    queryAll queryChunk
    where
        queryChunk start = query auth domain (SearchQuery jql start)
