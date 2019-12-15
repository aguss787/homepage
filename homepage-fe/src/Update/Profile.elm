module Update.Profile exposing (..)

import Http
import Message exposing (Message)
import Message.Profile as Profile
import Json.Decode exposing (string, list)
import Page as Page
import Model exposing (Model)

fetchCatImageUrl : Cmd Message
fetchCatImageUrl =
    Http.get
        { url = "http://localhost:8080/meme"
        , expect = Http.expectJson (\x -> Message.ProfileMessage (Profile.GotResult x)) (list string)
        }

update : Profile.Message -> Model -> (Model, Cmd msg)
update message model =
    case message of
        Profile.GotResult result ->
            case result of
                Ok imageUrl ->
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
                                    | imageUrl = imageUrl
                                    }
                                }
                            }
                    in
                    ( newModel, Cmd.none )

                Err err ->
                    ( {model | page = Page.Failure}, Cmd.none )