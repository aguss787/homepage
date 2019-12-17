module Utils.Directory where

import           Control.Monad.IO.Class
import qualified System.Directory       as Dir

class MonadIO m => DirIO m where
    listDir :: FilePath -> m [FilePath]

instance DirIO IO where
    listDir = Dir.listDirectory
