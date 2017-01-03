{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Jira2Sheet.Crypto where

import           Prelude                      hiding (readFile, take, writeFile)

import           Control.Exception            (Exception (..),
                                               SomeException (..))
import           Control.Monad.Except         (MonadError (..))
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaCha
import           Crypto.Error                 (CryptoError, eitherCryptoError)
import           Crypto.Scrypt                (Pass (..), PassHash (..),
                                               Salt (Salt), scrypt,
                                               scryptParamsLen)
import qualified Data.ByteArray               as B
import           Data.ByteArray.Pack          (fill, putBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Either.Combinators      (mapLeft)
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as Text
import           Text.Read                    (readMaybe)

import           Jira2Sheet.Common            (EncryptedData (..), Error (..),
                                               SavedCredentials (..), decode,
                                               encode, lazyDecode, lazyEncode,
                                               throwMessage)
import           Jira2Sheet.Types.Crypto      (MonadCrypto (..))
import           Jira2Sheet.Types.Files       (MonadReadFS (..),
                                               MonadWriteFS (..))
import           Jira2Sheet.Types.Input       (MonadInput (..), getPassword')
import           Jira2Sheet.Types.Log         (Log (..))


chachaEncrypt = ChaCha.encrypt :: ByteString -> ChaCha.State -> (ByteString, ChaCha.State)
chachaDecrypt = ChaCha.decrypt :: ByteString -> ChaCha.State -> (ByteString, ChaCha.State)

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
    liftCrypto $ crypt chachaEncrypt nonce key $ encode $ show creds

encryptCredentials' :: (MonadCrypto m, MonadError SomeException m) => String -> SavedCredentials -> m LBS.ByteString
encryptCredentials' password creds = lazyEncode . show <$> encryptCredentials password creds


crypt :: (ByteString -> ChaCha.State -> (ByteString, ChaCha.State)) -- en/decrypter
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

decryptCredentials :: (MonadCrypto m, MonadError SomeException m, MonadInput m) => String -> EncryptedData -> m SavedCredentials
decryptCredentials password ed@(EncryptedData nonce cyphertext auth) = do
    key <- stretchKey $ encode password
    (EncryptedData _ plaintext auth') <- liftCrypto $ crypt chachaDecrypt nonce key cyphertext
    if auth /= auth' then do
        password' <- getPassword' "Incorrect password, please insert again>"
        decryptCredentials password' ed
    else maybe (throwMessage "I'm sorry, it looks like your saved credentials are corrupted")
                pure $ readMaybe $ decode plaintext

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
