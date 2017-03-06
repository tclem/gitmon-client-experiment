module Main where

import Control.Monad.Reader
import Client

import Git.Libgit2
import Git.Repository
import Git.Blob
import Git.Types
import Git.Libgit2.Backend
import qualified Git

import System.Directory

main :: IO ()
main = do
 pwd <- getCurrentDirectory
 withRepository lgFactory pwd $ do
   repo <- getRepository
   lift $ runReaderT lookupCommit repo

  where
    lookupCommit = do
      object <- parseObjOid (pack "")
      commit <- reportGitmon "cat-file" $ lookupCommit object
      pure commit
  -- runReaderT someFunc "xyz"
