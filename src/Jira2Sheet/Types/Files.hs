module Jira2Sheet.Types.Files where

import           Prelude                   hiding (readFile, writeFile)

import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Log         (LoggingT (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy      as LBS
import qualified System.Directory          as Directory
import           UnexceptionalIO           (UIO, unsafeFromIO)


class (Monad m) => MonadWriteFS m where
    writeFile :: FilePath -> LBS.ByteString -> m ()

class (Monad m) => MonadReadFS m where
    readFile :: FilePath -> m LBS.ByteString
    doesFileExist :: FilePath -> m Bool

instance MonadWriteFS UIO where
    writeFile path = unsafeFromIO . LBS.writeFile path

instance MonadReadFS UIO where
    readFile = unsafeFromIO . LBS.readFile
    doesFileExist = unsafeFromIO . Directory.doesFileExist


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

instance (MonadWriteFS m) => MonadWriteFS (MaybeT m) where
    writeFile path = lift . writeFile path

instance (MonadReadFS m) => MonadReadFS (MaybeT m) where
    readFile = lift . readFile
    doesFileExist = lift . doesFileExist
