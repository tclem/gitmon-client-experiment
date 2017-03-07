module Main where

import Control.Monad.Reader
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Git.Libgit2
import Git.Repository
import Git.Blob
import Git.Types
import Git.Libgit2.Backend
import qualified Git

import Client

main :: IO ()
main = do
  pwd <- getCurrentDirectory

  withRepository lgFactory pwd $ do
    repo <- getRepository
    authorName <- withGitmon findAuthor
    liftIO $ T.putStrLn authorName

  -- withRepositoryAndSocket lgFactory pwd $ do
  --   repo <- getRepository
  --   authorName <- lift $ runReaderT findAuthor repo
  --   liftIO $ T.putStrLn authorName
  --
  putStrLn "Done."

  where
    findAuthor :: ReaderT LgRepo IO T.Text
    findAuthor = do
      object <- parseObjOid (T.pack "5e17e6bc4722fff0b5d946e56b6c44963aa9617e")
      commit <- lookupCommit object
      pure $ signatureName (commitAuthor commit)
