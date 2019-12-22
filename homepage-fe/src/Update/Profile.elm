module Update.Profile exposing (..)

import Http
import Message exposing (Message)
import Message.Profile as Profile
import Model.Profile as Profile
import Page as Page
import Model exposing (Model)

fetchCatImageUrl : Cmd Message
fetchCatImageUrl =
    Http.get
        --{ url = "https://profile.agus.dev/profile"
        { url = "http://localhost:8080/profile"
        , expect = Http.expectJson (\x -> Message.ProfileMessage (Profile.GotResult x)) Profile.completeProfileDecoder
        }

update : Profile.Message -> Model -> (Model, Cmd msg)
update message model =
    case message of
        Profile.GotResult result ->
            case result of
                Ok profile ->
                    let
                        oldModel = model.model
                        oldProfile = oldModel.profile
                        newModel =
                            { model
                            | page = Page.Profile
                            , model =
                                { oldModel
                                | profile =
                                    { oldProfile
                                    | profile = profile
                                    }
                                }
                            }
                    in
                    ( newModel, Cmd.none )

                Err err ->
                    ( {model | page = Page.Failure}, Cmd.none )