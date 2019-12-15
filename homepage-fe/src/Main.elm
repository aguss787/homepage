module Main exposing (main)

import Browser exposing (..)
import Browser.Navigation as Navigation
import Url exposing (Url)
import Html exposing (text)
import Utils.View.Adaptor exposing (fromAdaptor, toAdaptor)
import View.Profile as Profile
import Update.Profile as Profile
import Message.Routing as Routing
import Update.Routing as Routing
import View.Container exposing (container)
import Message exposing (Message)
import Page as Page
import Model exposing (Model, emptySpecificModel)

view : Model -> Document Message
view model =
    fromAdaptor
        <| container
        <| toAdaptor
            model
            <| case model.page of
                Page.Loading ->
                    { title = "Loading"
                    , body = [text "loading..."]
                    }

                Page.Failure ->
                    { title = "Error"
                    , body = [text "failed to fetch new cat image"]
                    }

                Page.Profile ->
                    Profile.view model.model.profile

init : () -> Url -> Navigation.Key -> ( Model, Cmd Message )
init _ url key =
    ( {navKey = key, model = emptySpecificModel, page = Page.Loading}, Profile.fetchCatImageUrl )


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Message.ProfileMessage message ->
            Profile.update message model

        Message.RoutingMessage message ->
            Routing.update message model

        _ -> (model, Cmd.none)

onUrlRequest : UrlRequest -> Message
onUrlRequest urlRequest =  case urlRequest of
    Internal _ -> Message.None
    External url -> Message.RoutingMessage (Routing.ExternalRequest url)

main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = \url -> Debug.log ("onchange " ++ Url.toString url) Message.None
        }