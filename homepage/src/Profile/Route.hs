{-# LANGUAGE OverloadedStrings #-}

module Profile.Route
    ( route
    )
where

import           Control.Monad.IO.Class
import           Meme.Internal.Getter
import           Utils.Env                     as Env
import           Web.Scotty                    as S
import qualified Profile.Internal.Model        as Model
import qualified Profile.Internal.Handler      as Handler
import           Database.Persist.Sql

route :: Env -> S.ScottyM ()
route env = do
    let pool = Env.dbPool env
    S.get "/profile/" $ do
        (=<<) S.json $ liftIO $ Handler.getProfile pool
