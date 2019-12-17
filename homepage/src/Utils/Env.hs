module Utils.Env
    ( Env(..)
    )
where

import           Database.Persist.Sql
import           Utils.Config         as Config

data Env = Env
    { config :: Config.Config
    , dbPool :: ConnectionPool
    }
