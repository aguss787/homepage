{-# LANGUAGE OverloadedStrings #-}

module WebService
    ( webService
    )
where


import qualified Web.Scotty                    as S
import qualified Data.Text.Lazy                as T
import           Control.Monad.IO.Class
import qualified Meme.Route                    as Meme

webService :: IO ()
webService = S.scotty 8080 $ do
    Meme.route
    S.get "/" $ do
        S.text "test"
    S.get "/test/:asd" $ do
        arg <- S.param "asd" :: S.ActionM Int
        S.text . T.pack $ show arg
    S.get "/test/:asd" $ do
        arg <- S.param "asd" :: S.ActionM T.Text
        S.text $ "other | " <> arg
