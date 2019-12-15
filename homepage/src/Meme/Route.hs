{-# LANGUAGE OverloadedStrings #-}

module Meme.Route
    ( route
    )
where

import           Web.Scotty                    as S
import           Control.Monad.IO.Class
import           Meme.Internal.Getter

route :: S.ScottyM ()
route = do
    S.get "/meme/" $ do
        S.json =<< liftIO getMemes
    S.get "/meme/:filename" $ do
        filename <- param "filename"
        (=<<) S.file $ liftIO . getMeme $ filename
