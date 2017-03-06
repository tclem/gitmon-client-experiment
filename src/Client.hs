module Client where

import Control.Monad.Reader
import Data.Monoid

someFunc :: ReaderT String IO ()
someFunc = do
  content <- ask
  liftIO $ putStrLn ("The Reader Content: " <> content)
