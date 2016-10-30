{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Prelude                      hiding (log)

import           Control.Monad                (void)
import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.Log            (MonadLog)
import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Aeson                   (FromJSON)
import           Data.ByteString              (ByteString)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Network.HTTP.Client.Internal (CookieJar (..), Response (..),
                                               ResponseClose (..))
import           Network.HTTP.Types.Status    (ok200)
import           Network.HTTP.Types.URI       (SimpleQuery, parseSimpleQuery)
import           Network.HTTP.Types.Version   (HttpVersion (..))
import           Network.URI                  (URI (..), parseURI)
import           Network.Wreq                 (basicAuth)
import           Test.Hspec
import           Text.Read                    (readMaybe)
import           Unsafe.Coerce                (unsafeCoerce)

import           Lib

mkFixture "FixtureInst" [
    ''MonadHTTPGet
  , ''MonadThrow
  , ''Log
  , ''MonadInput]


urlString2Query :: String -> Maybe SimpleQuery
urlString2Query = fmap (parseSimpleQuery . Text.encodeUtf8 . Text.pack . uriQuery) . parseURI

dummyIssue :: Int -> JiraIssue
dummyIssue n =
    JiraIssue{
        fields=JiraFields{
            status=JiraStatus{name="In Progress"}
          , summary="issue name"
          , customfield_10007=Nothing}
      , key="DEV-" <> (Text.pack $ show n)}

dummyIssues :: [JiraIssue]
dummyIssues = map dummyIssue [1..10]

dummyResponse :: [JiraIssue] -> Int -> JiraResponse
dummyResponse allIssues start =
    JiraResponse{
        maxResults=4
      , total=length allIssues
      , issues=take 4 $ drop start allIssues
    }

mockResponse :: a -> Response a
mockResponse body = Response ok200 (HttpVersion 1 2) [] body (CJ []) (ResponseClose $ pure ())

readMaybe' :: (Read a) => ByteString -> Maybe a
readMaybe' s = readMaybe $ Text.unpack $ Text.decodeUtf8 s

startParam :: String -> Maybe Int
startParam query = readMaybe' =<< lookup "startAt" =<< urlString2Query query

response :: (FromJSON a, Monad m) => String -> m (Response a)
response query = responseBuilder $ startParam query
    where
        responseBuilder (Just start) = pure $ mockResponse $ unsafeCoerce $ dummyResponse dummyIssues start
        responseBuilder Nothing = error $ "wrong query: " <> query

ignoreLogging = def {
    _logDebug = void . pure
  , _logInfo = void . pure
}

main :: IO ()
main = hspec $ do
    describe "queryAll" $
        it "queries all 10 issues, 4 by 4" $ do
            let queryInst = ignoreLogging {
                _getWith = \_ query -> log (startParam query) >>
                                       response query
            }
            let query' start = query (basicAuth "" "") (DomainName "") (SearchQuery (JQL "") start)
            let calls = logTestFixture (queryAll query') queryInst
            catMaybes calls `shouldBe` [0, 4, 8, 10]
    describe "getCredentials" $
        it "" $ do
            let credentialsInst = ignoreLogging {
                _getInputLine = \_ -> pure "INPUT"
              , _getPassword = \_ _ -> pure "PASSWORD"
            }
            let calls = logTestFixture getCredentials credentialsInst
            calls `shouldBe` []
