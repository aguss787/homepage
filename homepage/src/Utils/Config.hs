module Utils.Config
    ( ConfigHandler(..)
    , Config(..)
    , MemeConfig(..)
    )
where

import           System.Directory
import           Control.Exception
import           Data.Functor.Identity
import           Utils.Config.Internal as Internal
import           Control.Monad.IO.Class

class MonadIO m => ConfigHandler m where
    getMemeDir :: m FilePath

instance ConfigHandler IO where
    getMemeDir = Internal.getMemeDir
