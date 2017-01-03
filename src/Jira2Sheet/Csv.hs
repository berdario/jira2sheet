{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Jira2Sheet.Csv where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Csv             (DefaultOrdered, ToField (toField),
                                       ToNamedRecord,
                                       encodeDefaultOrderedByName)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           Network.URI          (URI (..))

import           Jira2Sheet.Common    (DomainName, encode)
import           Jira2Sheet.Jira      (JiraFields (..), JiraIssue (..),
                                       JiraStatus (..), jiraBrowse)

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

encodeToCsv :: DomainName -> [JiraIssue] -> LBS.ByteString
encodeToCsv domain issues = encodeDefaultOrderedByName $ map (jiraToRecord domain) issues
