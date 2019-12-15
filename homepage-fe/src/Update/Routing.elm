module Update.Routing exposing (..)

import Browser.Navigation as Navigation
import Message.Routing as Routing

update : Routing.Message -> model -> (model, Cmd msg)
update message model =
    case message of
        Routing.ExternalRequest url ->
            (model, Navigation.load url)