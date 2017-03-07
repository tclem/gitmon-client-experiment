{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Client where

import Control.Monad.Reader
import Data.Monoid
import Data.ByteString
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T

import Git.Libgit2
import Git.Repository
import Git.Blob
import Git.Types
import Git.Libgit2.Backend
import qualified Git

import Control.Exception
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.Timeout

-- Outer context is IO. Single socket opened for repository commands.
-- withRepository' :: (MonadGit r n, MonadBaseControl IO n, MonadIO m) => RepositoryFactory n m r -> FilePath -> n a -> m a
withRepositoryAndSocket :: RepositoryFactory (ReaderT LgRepo IO) IO LgRepo -> FilePath -> ReaderT LgRepo IO a -> IO a
withRepositoryAndSocket factory path f = withSocket $ \s -> do
  sendAll s $ encodeUtf8 (T.pack "update")
  !result <- withRepository factory path f
  sendAll s $ encodeUtf8 (T.pack "finish")
  pure result

-- Outer context is ReaderT LgRepo IO a.
withRepositoryAndSocket' :: RepositoryFactory (ReaderT LgRepo IO) IO LgRepo -> FilePath -> ReaderT LgRepo IO a -> IO a
withRepositoryAndSocket' factory path f = withRepository factory path $ do
  repo <- getRepository
  liftIO . withSocket $ \s -> do
    sendAll s $ encodeUtf8 (T.pack "update")
    !r <- runReaderT f repo
    sendAll s $ encodeUtf8 (T.pack "finish")
    pure r

withGitmon' :: ReaderT LgRepo IO a -> LgRepo -> IO a
withGitmon' f repo = withSocket $ \s -> do
  sendAll s $ encodeUtf8 (T.pack "update")
  !r <- runReaderT f repo
  sendAll s $ encodeUtf8 (T.pack "finish")
  pure r

withGitmon :: ReaderT LgRepo IO a -> ReaderT LgRepo IO a
withGitmon gitCommand = do
  safeIO $ withSocket (\s -> sendAll s $ encodeUtf8 (T.pack "update"))
  !r <- gitCommand
  safeIO $ withSocket (\s -> sendAll s $ encodeUtf8 (T.pack "finished"))
  pure r

withSocket :: (Socket -> IO c) -> IO c
withSocket = bracket connectSocket close
  where
    connectSocket = do
      s <- socket AF_UNIX Stream defaultProtocol
      connect s (SockAddrUnix "/tmp/gitstats.sock")
      pure s

safeIO :: MonadIO m => IO a -> m (Maybe a)
safeIO command = liftIO $ (Just <$> command) `catch` noop

noop :: IOException -> IO (Maybe a)
noop _ = pure Nothing
