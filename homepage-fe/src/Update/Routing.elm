module Update.Routing exposing (..)

import Browser.Navigation as Navigation
import Message.Routing as Routing
import Model exposing (Model)
import Maybe exposing (withDefault)

update : Routing.Message -> Model -> (Model, Cmd msg)
update message model =
    case message of
        Routing.ExternalRequest url ->
            (model, Navigation.load url)
        Routing.InternalRequest url ->
            (model, Navigation.pushUrl model.navKey <| "#" ++ withDefault "" url.fragment)