module Utils.Config
    ( ConfigHandler(..)
    , Config(..)
    , MemeConfig(..)
    , DBConfig(..)
    , getDbConnectionString
    )
where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           System.Directory
import           Utils.Config.Internal  as Internal

class MonadIO m => ConfigHandler m where
    getConfig  :: m Config
    getMemeDir :: m FilePath

instance ConfigHandler IO where
    getMemeDir = Internal.getMemeDir
    getConfig  = Internal.getConfig

connStr = "host=localhost dbname=profile user=postgres password=postgres port=5432"
getDbConnectionString :: Config -> String
getDbConnectionString _ = connStr
