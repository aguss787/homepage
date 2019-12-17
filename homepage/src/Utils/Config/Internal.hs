{-# LANGUAGE DeriveGeneric #-}

module Utils.Config.Internal where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Yaml              as Yaml
import           GHC.Generics

data Config = Config
    { meme :: MemeConfig
    , db   :: DBConfig
    } deriving (Generic, Show, Eq)

instance FromJSON Config

newtype MemeConfig = MemeConfig
    { memeDir :: String
    } deriving (Generic, Show, Eq)

instance FromJSON MemeConfig

data DBConfig = DBConfig
    { host     :: String
    , port     :: Int
    , username :: String
    , password :: String
    , database :: String
    } deriving (Generic, Show, Eq)

instance FromJSON DBConfig

class (MonadIO m) => ConfigIO m where
    decodeFileEither :: FilePath
                        -> m (Either Yaml.ParseException Config)

instance ConfigIO IO where
    decodeFileEither = Yaml.decodeFileEither

getConfigFilePath :: FilePath
getConfigFilePath = "data/config.yaml"

getConfig :: ConfigIO m => m Config
getConfig = either (error . show) id <$> decodeFileEither getConfigFilePath

getMemeDir :: ConfigIO m => m FilePath
getMemeDir = (meme >>> memeDir) <$> getConfig

getDBConfig :: ConfigIO m => m DBConfig
getDBConfig = db <$> getConfig
