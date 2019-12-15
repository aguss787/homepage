module Utils.Routing.UrlRequest exposing (..)

import Browser exposing (..)
import Url

toString urlReq = case urlReq of
    Internal url -> "Internal " ++ Url.toString url
    External url -> "External " ++ url