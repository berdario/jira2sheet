{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Jira2Sheet.Types.Crypto where

import           Control.Exception         (SomeException)
import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Log         (LoggingT)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Crypto.Random.Entropy     as Entropy
import           Data.ByteString           (ByteString)
import           Network.HostName          (getHostName)
import           UnexceptionalIO           (UIO, fromIO, unsafeFromIO)

import           Jira2Sheet.Common         (encode)

class (Monad m) => MonadCrypto m where
    getSalt :: m ByteString
    getEntropy :: Int -> m ByteString

instance (MonadTrans t, Monad (t UIO)) => MonadCrypto (ExceptT SomeException (t UIO)) where
    getSalt = lift $ lift $ unsafeFromIO $ fmap encode getHostName
    getEntropy = ExceptT . lift . fromIO . Entropy.getEntropy

instance (MonadCrypto m) => MonadCrypto (LoggingT message m) where
    getSalt = lift getSalt
    getEntropy = lift . getEntropy
