{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module WebService
    ( webService
    )
where


import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.UTF8          as BSU
import qualified Data.Text.Lazy                as T
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import qualified Meme.Route                    as Meme
import qualified Profile.Route                 as Profile
import qualified Utils.Config                  as Config
import           Utils.Env
import qualified Web.Scotty                    as S

import qualified Database.Persist as P

webService :: IO ()
webService = do
    config <- Config.getConfig

    let connStr = BSU.fromString $ Config.getDbConnectionString config
    pool <-
        (runStderrLoggingT $ createPostgresqlPool connStr 10) :: IO
            ConnectionPool

    let env = Env { config = config, dbPool = pool }

    S.scotty 8080 $ do
        Meme.route env
        Profile.route env
        S.get "/ping" $ do
            S.text "test"
