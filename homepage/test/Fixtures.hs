module Fixtures where

import qualified Utils.Config as Config
import qualified Utils.Env    as Env

directory :: FilePath
directory = "meme/dir"

directoryContent :: [FilePath]
directoryContent = ["a", "b", "c"]

file :: FilePath
file = head directoryContent

config :: Config.Config
config = Config.Config
    { Config.meme = Config.MemeConfig
        { Config.memeDir = directory
        }
    , Config.db = Config.DBConfig
            { Config.host = "host"
            , Config.port = 5432
            , Config.username = "user"
            , Config.password = "pass"
            , Config.database = "db"
            }
    }

env :: Env.Env
env = Env.Env
    { Env.config = config
    , Env.dbPool = undefined
    }
