{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Prelude                      hiding (log)

import           Control.Applicative          (Alternative (..))
import           Control.Exception            (Exception (..), SomeException)
import           Control.Monad                (MonadPlus (..), void)
import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.Log            (MonadLog)
import           Control.Monad.State.Class    (MonadState, gets, modify)
import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Control.Monad.Trans.Maybe    (MaybeT)
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
import           Network.OAuth.OAuth2         (AccessToken (..))
import           Network.URI                  (URI (..), parseURI)
import           Network.Wreq                 (basicAuth)
import           Test.Hspec
import           Text.Read                    (readMaybe)
import           Unsafe.Coerce                (unsafeCoerce)

import           Lib

mkFixture "FixtureInst" [ts|
      MonadHTTPGet
    , MonadThrow
    , Log
    , MonadInput
    , MonadWriteFS
    , MonadReadFS
    , MonadOAuth
    , MonadCrypto
    |]

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

ignoreLogging :: (Applicative m) => FixtureInst m
ignoreLogging = def {
    _logDebug = void . pure
  , _logInfo = void . pure
}

data Event = Authorize | Refresh | Write FilePath deriving (Eq, Show)

fakeToken = AccessToken "" (Just "") Nothing Nothing Nothing
fakeCredentials = SavedCredentials "" "" ""

fakeDecryptCredentials :: (MonadState [Event] m) => MaybeT m SavedCredentials
fakeDecryptCredentials = do
    saved <- gets $ elem $ Write "credentials.enc"
    if saved
        then pure fakeCredentials
        else mzero

logEvent :: MonadState [Event] m => Event -> m ()
logEvent e = modify (e:)

shouldHaveCalled :: (Eq a, Show a, Exception e) => Either e ([a], t) -> [a] -> Expectation
shouldHaveCalled (Right (calls, _)) expected = reverse calls `shouldBe` expected
shouldHaveCalled (Left e) _ = expectationFailure $ displayException e

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
        it "After saving credentials, authorization is not performed again" $ do
            let credentialsInst = ignoreLogging {
                _getInputLine = \_ -> pure "INPUT"
              , _getPassword = \_ _ -> pure "PASSWORD"
              , _oauthAuthorize = \_ _ _ -> logEvent Authorize >> pure fakeToken
              , _fetchRefreshToken = \_ _ _ -> logEvent Refresh >> pure fakeToken
              , _decryptSavedCredentials = \_ _ -> fakeDecryptCredentials
              , _writeFile = const . logEvent . Write
              , _getEntropy = const $ pure "000011112222"
              , _getSalt = pure "salt"
            }
            let getCredentials' = getCredentials undefined undefined
            let result = execTestFixtureT (getCredentials' >> getCredentials') credentialsInst []
            result `shouldHaveCalled` [Authorize, Write "credentials.enc", Refresh]
