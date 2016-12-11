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
      , MonadHTTPGet
      , MonadWriteFS
      , MonadReadFS
      , Log
      , MonadInput
      , MonadOAuth
      , MonadCrypto(..)
      , MonadError
      , query
      , queryAll
      , getCredentials
      , encryptCredentials'
      , crypt
      , stretchKey
      , liftCrypto
      , chachaEncrypt
      , chachaDecrypt
      , DomainName(..)
      , JQL(..)
      , SearchQuery(..)
      , JiraStatus(..)
      , JiraFields(..)
      , JiraIssue(..)
      , JiraResponse(..)
      , Credentials(..)
      , SavedCredentials(..)
      , EncryptedData(..)
    ) where

import           Prelude                      hiding (readFile, take, writeFile)

import           Control.Applicative          (empty)
import           Control.Exception            (Exception (..),
                                               SomeException (..))
import           Control.Lens                 ((&), (.~), (?~), (^.))
import           Control.Monad                (MonadPlus, join, unless, (<=<))
import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.Except         (ExceptT (..), MonadError (..),
                                               mapExceptT, runExceptT,
                                               withExceptT)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Log            (LoggingT (..), WithSeverity,
                                               runLoggingT)
import qualified Control.Monad.Log            as Log
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Maybe    (MaybeT (..), mapMaybeT,
                                               maybeToExceptT)
import           Control.Monad.Trans.Reader   (ReaderT (..))
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaCha
import           Crypto.Error                 (CryptoError, eitherCryptoError)
import qualified Crypto.Random.Entropy        as Entropy
import           Crypto.Scrypt                (Pass (..), PassHash (..),
                                               Salt (Salt), scrypt,
                                               scryptParamsLen)
import           Data.Aeson                   (FromJSON (..), Value (..), (.:))
import qualified Data.ByteArray               as B
import           Data.ByteArray.Pack          (fill, putBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Csv                     (DefaultOrdered,
                                               ToField (toField), ToNamedRecord,
                                               encodeDefaultOrderedByName)
import           Data.Either.Combinators      (mapLeft)
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as LazyText
import qualified Data.Text.Lazy.Encoding      as LazyText
import           GHC.Generics                 (Generic)
import           Network.HostName             (getHostName)
import           Network.HTTP.Conduit         (Manager, ManagerSettings,
                                               newManager, tlsManagerSettings)
import           Network.HTTP.Types.URI       (urlEncode)
import           Network.OAuth.OAuth2         (AccessToken (..), OAuth2 (..),
                                               QueryParams, appendQueryParam,
                                               authorizationUrl,
                                               fetchAccessToken)
import qualified Network.OAuth.OAuth2         as OAuth2
import           Network.URI                  (URI (..), URIAuth (..))
import           Network.Wreq                 (Auth, asJSON, auth, defaults,
                                               header, param, responseBody)
import qualified Network.Wreq                 as Wreq
import           Network.Wreq.Types           (Postable)
import           System.Console.Haskeline     (InputT, MonadException (..),
                                               RunIO (..), defaultSettings,
                                               runInputT)
import qualified System.Console.Haskeline     as Haskeline
import qualified System.Directory             as Directory
import           Text.Read                    (readMaybe)
import           UnexceptionalIO              (UIO, fromIO, unsafeFromIO)
import           Web.Browser                  (openBrowser)

chachaEncrypt = ChaCha.encrypt :: ByteString -> ChaCha.State -> (ByteString, ChaCha.State)
chachaDecrypt = ChaCha.decrypt :: ByteString -> ChaCha.State -> (ByteString, ChaCha.State)

class (Monad m) => Log m where
     logDebug :: Text -> m ()
     logInfo :: Text -> m ()
     logError :: Text -> m ()

data Error = Error String deriving (Show, Eq)
deriving instance Exception Error

class (Monad m ) => MonadHTTPGet m where -- TODO add back MonadThrow, change to MonadError
    getWith :: (FromJSON a) => Wreq.Options -> String -> m (Wreq.Response a)

class (Monad m) => MonadHTTP m where
    postWith :: (Postable a, FromJSON b) => Wreq.Options -> String -> a -> m (Wreq.Response b)

class (Monad m) => MonadWriteFS m where
    writeFile :: FilePath -> LBS.ByteString -> m ()

class (Monad m) => MonadReadFS m where
    readFile :: FilePath -> m LBS.ByteString
    doesFileExist :: FilePath -> m Bool

class (Monad m) => MonadInput m where
    getInputLine :: String -> m String
    getPassword :: Maybe Char -> String -> m String

getPassword' :: (MonadInput m) => String -> m String
getPassword' = getPassword (Just '*')

class (Monad m) => MonadOAuth m where
    newTls :: ManagerSettings -> m Manager
    oauthAuthorize :: Manager -> OAuth2 -> QueryParams -> m AccessToken
    fetchRefreshToken :: Manager -> OAuth2 -> ByteString -> m AccessToken

class (Monad m) => MonadCrypto m where
    getSalt :: m ByteString
    getEntropy :: Int -> m ByteString

instance (MonadTrans t, Monad (t UIO)) => MonadCrypto (ExceptT SomeException (t UIO)) where
    getSalt = lift $ lift $ unsafeFromIO $ fmap encode getHostName
    getEntropy = ExceptT . lift . fromIO . Entropy.getEntropy


instance (MonadCrypto m) => MonadCrypto (LoggingT message m) where
    getSalt = lift getSalt
    getEntropy = lift . getEntropy

instance (Monad m) => Log (LoggingT (WithSeverity Text) m) where
    logDebug = Log.logDebug
    logInfo = Log.logInfo
    logError = Log.logError

instance (Log m) => Log (ExceptT e m) where
    logDebug = lift . logDebug
    logInfo = lift . logInfo
    logError = lift . logError

instance MonadHTTPGet UIO where
    getWith options = unsafeFromIO . (asJSON <=< Wreq.getWith options)

instance MonadHTTP UIO where
    postWith options body = unsafeFromIO . (asJSON <=< Wreq.postWith options body)

instance MonadWriteFS UIO where
    writeFile path = unsafeFromIO . LBS.writeFile path

instance MonadReadFS UIO where
    readFile = unsafeFromIO . LBS.readFile
    doesFileExist = unsafeFromIO . Directory.doesFileExist

runInput :: InputT IO (Maybe a) -> MaybeT UIO a -- TODO add back MonadException, convert to MonadError?
runInput = MaybeT . unsafeFromIO . runInputT defaultSettings

instance MonadInput (MaybeT UIO) where
    getInputLine = runInput . Haskeline.getInputLine
    getPassword c = runInput . Haskeline.getPassword c

encode :: String -> ByteString
encode = Text.encodeUtf8 . Text.pack

decode :: ByteString -> String
decode = Text.unpack . Text.decodeUtf8

lazyEncode :: String -> LBS.ByteString
lazyEncode = LazyText.encodeUtf8 . LazyText.pack

lazyDecode :: LBS.ByteString -> String
lazyDecode = LazyText.unpack . LazyText.decodeUtf8

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

instance (MonadWriteFS m) => MonadWriteFS (LoggingT message m) where
    writeFile path = lift . writeFile path

instance (MonadWriteFS m) => MonadWriteFS (ExceptT e m) where
    writeFile path = lift . writeFile path

instance (MonadReadFS m) => MonadReadFS (LoggingT message m) where
    readFile = lift . readFile
    doesFileExist = lift . doesFileExist

instance (MonadReadFS m) => MonadReadFS (ExceptT e m) where
    readFile = lift . readFile
    doesFileExist = lift . doesFileExist

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

instance (MonadWriteFS m) => MonadWriteFS (MaybeT m) where
    writeFile path = lift . writeFile path

instance (MonadReadFS m) => MonadReadFS (MaybeT m) where
    readFile = lift . readFile
    doesFileExist = lift . doesFileExist

instance (MonadInput m) => MonadInput (LoggingT message m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

instance (MonadInput m) => MonadInput (ExceptT e m) where
    getInputLine = lift . getInputLine
    getPassword c = lift . getPassword c

type RefreshToken = ByteString
type JiraUsername = String
type JiraPassword = String
data SavedCredentials = SavedCredentials JiraUsername JiraPassword RefreshToken deriving (Show, Read)
data Credentials = Credentials Auth AccessToken

data EncryptedData = EncryptedData ByteString ByteString ByteString deriving (Show, Read)

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

data DriveFileMetadata = DriveFileMetadata {
    fileId :: Text
} deriving (Show, Eq)

instance FromJSON DriveFileMetadata where
    parseJSON (Object v) = DriveFileMetadata <$> v .: "id"
    parseJSON invalid = empty -- typeMismatch "DriveFileMetadata" invalid

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

getJiraCredentials :: (MonadInput m) => m (JiraUsername, JiraPassword)
getJiraCredentials = do
    user <- getInputLine "Jira username>"
    password <- getPassword' "Jira password>"
    return (user, password)

scryptParams = fromJust $ scryptParamsLen 19 8 1 32

stretchKey :: (MonadCrypto m, MonadError SomeException m) => ByteString -> m B.ScrubbedBytes
stretchKey pass = do
    salt <- getSalt
    liftCrypto $ toScrubbed $ getHash $ scrypt scryptParams (Salt salt) (Pass pass)

toScrubbed :: ByteString -> Either Error B.ScrubbedBytes
toScrubbed bs = mapLeft Error $ fill l $ putBytes bs
    where
        l = BS.length bs

liftCrypto :: (Exception e, MonadCrypto m, MonadError SomeException m) => Either e a -> m a
liftCrypto = either (throwError . SomeException) pure

encryptCredentials :: (MonadCrypto m, MonadError SomeException m) => String -> SavedCredentials -> m EncryptedData
encryptCredentials password creds = do
    nonce <- getEntropy 12
    key <- stretchKey $ encode password
    liftCrypto $ crypt ChaCha.encrypt nonce key $ encode $ show creds

encryptCredentials' :: (MonadCrypto m, MonadError SomeException m) => String -> SavedCredentials -> m LBS.ByteString
encryptCredentials' password creds = lazyEncode . show <$> encryptCredentials password creds

throwMessage :: (MonadError SomeException m) => String -> m a
throwMessage = throwError . SomeException . Error

decryptCredentials :: (MonadCrypto m, MonadError SomeException m, MonadInput m) => String -> EncryptedData -> m SavedCredentials
decryptCredentials password ed@(EncryptedData nonce cyphertext auth) = do
    key <- stretchKey $ encode password
    (EncryptedData _ plaintext auth') <- liftCrypto $ crypt ChaCha.decrypt nonce key cyphertext
    if auth /= auth' then do
        password' <- getPassword' "Incorrect password, please insert again>"
        decryptCredentials password' ed
    else maybe (throwMessage "I'm sorry, it looks like your saved credentials are corrupted")
                pure $ readMaybe $ decode plaintext

crypt
    :: (ByteString -> ChaCha.State -> (ByteString, ChaCha.State)) -- en/decrypter
    -> ByteString -- nonce (12 random bytes)
    -> B.ScrubbedBytes -- symmetric key
    -> ByteString -- input to be encrypted/decrypted
    -> Either CryptoError EncryptedData
crypt encrypter nonce key sourcetext = eitherCryptoError $ do
    st1 <- ChaCha.nonce12 nonce >>= ChaCha.initialize key
    let
        st2 = ChaCha.finalizeAAD $ ChaCha.appendAAD BS.empty st1
        (out, st3) = encrypter sourcetext st2
        auth = ChaCha.finalize st3
    return $ EncryptedData nonce out $ B.convert auth

saveCredentials :: (MonadWriteFS m, MonadInput m, MonadCrypto m, MonadError SomeException m) => SavedCredentials -> m ()
saveCredentials creds = do
    pass <- getPassword' "Please insert an encryption password for your credentials>"
    encryptedCreds <- encryptCredentials' pass creds
    writeFile "credentials.enc" encryptedCreds

authorizeAndSave :: (MonadOAuth m, MonadWriteFS m, MonadInput m, MonadCrypto m, MonadError SomeException m) => Manager -> OAuth2 -> (RefreshToken -> SavedCredentials) -> m AccessToken
authorizeAndSave mgr oauth builder = do
    googleRefresh <- oauthAuthorize mgr oauth [("scope", "https://www.googleapis.com/auth/drive")]
    traverse (saveCredentials . builder) $ refreshToken googleRefresh
    pure googleRefresh

maybeDecrypt :: (MonadInput m, MonadReadFS m, MonadCrypto m, MonadError SomeException m) => FilePath -> String -> m SavedCredentials
maybeDecrypt filepath password = do
    maybeEncryptedData <- readMaybe . lazyDecode <$> readFile filepath
    case maybeEncryptedData of
        (Just encryptedData) -> decryptCredentials password encryptedData
        Nothing -> throwMessage "It looks like your encrypted credentials are corrupted"


decryptSavedCredentials :: (MonadInput m, MonadReadFS m, MonadCrypto m, MonadError SomeException m, Log m) => FilePath -> m (Maybe SavedCredentials)
decryptSavedCredentials filepath = do
    fileExists <- doesFileExist filepath
    if fileExists then do
        password <- getPassword' "Please insert your credentials decryption password>"
        catchError (Just <$> maybeDecrypt filepath password) $ \e -> do
                logError $ Text.pack $ displayException e
                pure Nothing
    else pure Nothing


getCredentials :: (MonadInput m, MonadReadFS m, MonadOAuth m, MonadWriteFS m, MonadCrypto m, MonadError SomeException m, Log m) => Manager -> OAuth2 -> m Credentials
getCredentials mgr oauth = do
    credentials <- decryptSavedCredentials "credentials.enc" -- TODO `mplus` pure Nothing
    let authorizeAndSave' = authorizeAndSave mgr oauth
    case credentials of
        Nothing -> do
            (user, pass) <- getJiraCredentials
            Credentials (basicAuth user pass) <$> authorizeAndSave' (SavedCredentials user pass)
        (Just (SavedCredentials user pass tkn)) -> do
            logInfo "credentials decrypted"
            Credentials (basicAuth user pass) <$> fetchRefreshToken mgr oauth tkn
            -- tokenResponse <- runExceptT $ fetchRefreshToken mgr oauth tkn
            -- Credentials jCred <$> either (const $ authorizeAndSave' jCred) pure tokenResponse


newtype DomainName = DomainName String
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

uploadCsv :: (MonadHTTP m) => AccessToken -> LBS.ByteString -> m DriveFileMetadata
uploadCsv tkn content = do
    let opts = defaults & auth ?~ (Wreq.oauth2Bearer $ accessToken tkn)
                        & header "Content-Type" .~ ["text/csv"]
                        & param "uploadType" .~ ["media"]
    response <- postWith opts "https://www.googleapis.com/upload/drive/v3/files" content
    pure $ response ^. responseBody

jiraApp :: (MonadInput m, Log m, MonadHTTPGet m, MonadHTTP m, MonadWriteFS m, MonadReadFS m, MonadOAuth m, MonadCrypto m, MonadError SomeException m) =>
            (DomainName, JQL) -> m ()
jiraApp (domain, jql) = do
    mgr <- newTls tlsManagerSettings
    Credentials auth tkn <- getCredentials mgr oauth
    let queryChunk start = query auth domain (SearchQuery jql start)
    is <- queryAll queryChunk
    fileMetadata <- uploadCsv tkn $ encodeDefaultOrderedByName $ map (jiraToRecord domain) is
    logInfo $ "created https://drive.google.com/file/d/" <> fileId fileMetadata

oauth = OAuth2 { oauthClientId = "804038769221-786vn5l5m772h21masc5p4nm3gl995as.apps.googleusercontent.com"
               , oauthClientSecret = "0JuYNS6p7ibK8jA38_rJBkWO"
               , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
               , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
               , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v4/token" }
