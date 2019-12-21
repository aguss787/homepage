module Main exposing (main)

import Browser exposing (..)
import Url exposing (Url)
import Html exposing (text)
import Utils.View.Adaptor exposing (fromAdaptor, toAdaptor)
import View.Profile as Profile
import Update.Profile as Profile
import Message.Routing as Routing
import Update.Routing as Routing
import View.Container exposing (templateContainer)
import Message exposing (Message)
import Page as Page
import View.Loading as Loading
import Model exposing (Model)
import Init exposing (init)
import Bootstrap.Navbar as Navbar
import Message.Container as Container
import Update.Container as Container

view : Model -> Document Message
view model =
    fromAdaptor
        <| templateContainer
        <| toAdaptor
            model
            <| case model.page of
                Page.Loading ->
                    Loading.view

                Page.Failure ->
                    { title = "Error"
                    , body = [text "failed to fetch new cat image"]
                    }

                Page.Profile ->
                    Profile.view model.model.profile




update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Message.ProfileMessage message ->
            Profile.update message model

        Message.RoutingMessage message ->
            Routing.update message model

        Message.ContainerMessage message ->
            Container.update message model

        Message.None -> (model, Cmd.none)

onUrlRequest : UrlRequest -> Message
onUrlRequest urlRequest =  case urlRequest of
    Internal url -> Message.RoutingMessage (Routing.InternalRequest url)
    External url -> Message.RoutingMessage (Routing.ExternalRequest url)

subscriptions : Model -> Sub Message
subscriptions model =
    Navbar.subscriptions model.model.container.navbar (\x -> Message.ContainerMessage <| Container.NavbarMsg x)

main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = Message.RoutingMessage << Routing.InternalJump
        }