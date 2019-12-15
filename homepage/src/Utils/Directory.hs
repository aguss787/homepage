module Utils.Directory where

import qualified System.Directory              as Dir
import           Control.Monad.IO.Class

class MonadIO m => DirIO m where
    listDir :: FilePath -> m [FilePath]

instance DirIO IO where
    listDir = Dir.listDirectory