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
import           Text.Printf

class MonadIO m => ConfigHandler m where
    getConfig  :: m Config
    getMemeDir :: m FilePath

instance ConfigHandler IO where
    getMemeDir = Internal.getMemeDir
    getConfig  = Internal.getConfig

connStrFormat = "host=%s dbname=%s user=%s password=%s port=%d"
getDbConnectionString :: Config -> String
getDbConnectionString config = 
    printf connStrFormat host dbname user password port
    where
        dbConfig = db config
        host = Internal.host dbConfig
        dbname = Internal.database dbConfig
        user = Internal.username dbConfig
        password = Internal.password dbConfig
        port = Internal.port dbConfig
