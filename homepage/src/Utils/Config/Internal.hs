{-# LANGUAGE DeriveGeneric #-}

module Utils.Config.Internal where

import           GHC.Generics
import           Control.Monad.IO.Class
import           Control.Arrow
import qualified Data.Yaml                     as Yaml
import           Data.Aeson

newtype Config = Config {
    meme :: MemeConfig
} deriving (Generic, Show, Eq)

instance FromJSON Config

newtype MemeConfig = MemeConfig {
    memeDir :: String
} deriving (Generic, Show, Eq)

instance FromJSON MemeConfig

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
