{-# LANGUAGE OverloadedStrings #-}

module Meme.Route
    ( route
    )
where

import           Control.Monad.IO.Class
import           Meme.Internal.Getter
import           Utils.Env              as Env
import           Web.Scotty             as S

route :: Env.Env -> S.ScottyM ()
route env = do
    S.get "/meme/" $ do
        (=<<) S.json $ liftIO $ getMemes env
    S.get "/meme/:filename" $ do
        filename <- param "filename"
        S.file $ getMeme env filename
